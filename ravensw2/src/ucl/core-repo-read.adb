--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with System;

with Core.Config;
with Core.Event;
with Ucl;

package body Core.Repo.Read is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package EV  renames Core.Event;

   --------------------------------------------------------------------
   --  walk_repo_obj
   --------------------------------------------------------------------
   procedure walk_repo_obj (fileobj  : access constant libucl.ucl_object_t;
                            filename : String;
                            flags    : Init_protocol)
   is
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      item : access constant libucl.ucl_object_t;
   begin
      loop
         item := Ucl.ucl_object_iterate (fileobj, iter'Access, True);
         exit when item = null;

         declare
            --  key is the name of the repository, and source of the hash for repositories
            key    : String := Ucl.ucl_object_key (item);
            keystr : Text := SUS (key);
         begin
            EV.emit_debug (1, "RepoRead: parsing key " & SQ (key));
            if repositories.Contains (keystr) then
               EV.emit_debug (1, "RepoRead: overwriting repository " & key);
            end if;
            if Ucl.type_is_object (item) then
               add_repo (repo_obj => item, reponame => key, flags => flags);
            else
               EV.emit_error ("Ignoring bad configuration entry in " &
                                   filename & ": " & Ucl.ucl_emit_yaml (item));
            end if;
         end;
      end loop;
   end walk_repo_obj;


   --------------------------------------------------------------------
   --  load_repo_file
   --------------------------------------------------------------------
   procedure load_repo_file (dfd      : Unix.File_Descriptor;
                             repodir  : String;
                             repofile : String;
                             flags    : Init_protocol)
   is
      parser  : Ucl.T_parser;
      obj     : access libucl.ucl_object_t;
      key_ABI : constant String := Config.get_ci_key (Config.abi);
      myarch  : constant String := Config.configuration_value (config.abi);
      fd      : Unix.File_Descriptor;
      success : Boolean;
      res     : Boolean;
   begin
      parser := Ucl.ucl_parser_new_basic;
      Ucl.ucl_parser_register_variable (parser, key_ABI, myarch);
      EV.emit_debug (1, "RepoRead: loading " & SQ (repodir & "/" & repofile));
      fd := Unix.open_file (dfd, repofile, (RDONLY => True, others => False));

      if not Unix.file_connected (fd) then
         EV.emit_with_strerror ("Unable to open " & SQ (repodir & "/" & repofile));
         return;
      end if;

      success := Ucl.ucl_parser_add_fd (parser, fd);
      res := Unix.close_file (fd);

      if not success then
         EV.emit_with_strerror ("Error parsing: " & SQ (repodir & "/" & repofile));
         libucl.ucl_parser_free (parser);
         return;
      end if;

      obj := Ucl.ucl_parser_get_object (parser);
      libucl.ucl_parser_free (parser);

      if obj = null then
         return;
      end if;

      if Ucl.type_is_object (obj) then
         walk_repo_obj (obj, repofile, flags);
      end if;

      libucl.ucl_object_unref (obj);

   end load_repo_file;


   --------------------------------------------------------------------
   --  load_repo_files
   --------------------------------------------------------------------
   procedure load_repo_files (repodir : String; flags : Init_protocol)
   is
      package Tmp_Crate is new CON.Vectors
        (Element_Type => Text,
         Index_Type   => Natural,
         "="          => SU."=");

      procedure loadfile (position : Tmp_Crate.Cursor);
      procedure populate_priority (position : Repository_Crate.Cursor);

      fd : Unix.File_Descriptor;
      res : Boolean;

      procedure loadfile (position : Tmp_Crate.Cursor)
      is
         filename : String := USS (Tmp_Crate.Element (position));
      begin
         load_repo_file (dfd      => fd,
                         repodir  => repodir,
                         repofile => filename,
                         flags    => flags);
      end loadfile;

      procedure populate_priority (position : Repository_Crate.Cursor)
      is
         item    : A_repo renames Repository_Crate.Element (position);
         priorec : Repo_Priority;
      begin
         priorec.reponame := item.name;
         priorec.priority := item.priority;
         repositories_order.Append (priorec);
      end populate_priority;

   begin
      EV.emit_debug (1, "RepoRead: loading repositories in " & repodir);

      --  Don't bother to load in alphabetical order like pkg(8) does
      --  1) it loads into a hashed map which is a random order anyway
      --  2) pkg(8) has a bug where it ignores priority order.  We're going to fix
      --  that and sort by priority after loading.

      declare
         Search    : DIR.Search_Type;
         Dirent    : DIR.Directory_Entry_Type;
         tempstore : Tmp_Crate.Vector;
         use type DIR.File_Kind;
      begin
         if DIR.Exists (repodir) and then
           DIR.Kind (repodir) = DIR.Directory
         then
            DIR.Start_Search (Search    => Search,
                              Directory => repodir,
                              Filter    => (DIR.Ordinary_File => True, others => False),
                              Pattern   => "*.conf");

            while DIR.More_Entries (Search) loop
               DIR.Get_Next_Entry (Search => Search, Directory_Entry => Dirent);
               tempstore.Append (SUS (DIR.Simple_Name (Dirent)));
            end loop;
            DIR.End_Search (Search);

            fd := Unix.open_file (repodir, (DIRECTORY => True, CLOEXEC => True, others => False));
            if Unix.file_connected (fd) then
               tempstore.Iterate (loadfile'Access);
               res := Unix.close_file (fd);
            end if;
         end if;
      end;

      --  spin through repos and set priority order
      repositories.Iterate (populate_priority'Access);
      priority_sorter.Sort (repositories_order);
   end load_repo_files;


   --------------------------------------------------------------------
   --  add_repo
   --------------------------------------------------------------------
   procedure add_repo (repo_obj : access constant libucl.ucl_object_t;
                       reponame : String;
                       flags    : Init_protocol)
   is
      enabled      : access constant libucl.ucl_object_t;
      reponame_txt : Text := SUS (reponame);
      new_repo     : A_repo;
      found_repo   : Boolean;
      enable_repo  : Boolean;
   begin
      EV.emit_debug (1, "RepoRead: parsing repository object " & reponame);

      found_repo := repositories.Contains (reponame_txt);

      enabled := Ucl.ucl_object_find_key (repo_obj, "enabled");
      if enabled = null then
         enabled := Ucl.ucl_object_find_key (repo_obj, "ENABLED");
      end if;

      if enabled = null then
         enable_repo := True;
      else
         enable_repo := Ucl.ucl_object_toboolean (enabled);
         if found_repo and then not enable_repo then
            --  Remove the existing data and forget everything parsed so far
            EV.emit_debug (1, "RepoRead: disabling repo " & reponame);

            repositories.Delete (reponame_txt);
            return;
         end if;
      end if;

      declare
         iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
         item : access constant libucl.ucl_object_t;

         url          : Text;
         pubkey       : Text;
         mirror_type  : Text;
         sig_type     : Text;
         fingerprints : Text;
         use_ipvx     : int64;
         priority     : int64;
         env_obj      : access constant libucl.ucl_object_t;
      begin
         loop
            item := Ucl.ucl_object_iterate (repo_obj, iter'Access, True);
            exit when item = null;

            declare
               function content_is_string_type return Boolean;
               function content_is_integer_type return Boolean;
               function content_is_object_type return Boolean;
               procedure generic_error_emission (flavor : String);

               key  : String := Ucl.ucl_object_key (item);
               ukey : String := uppercase (key);

               procedure generic_error_emission (flavor : String) is
               begin
                  EV.emit_error ("Expecting " & flavor & " for the " & SQ (key) &
                                   " key of the " & SQ (reponame) & " repository");
               end generic_error_emission;

               function content_is_string_type return Boolean is
               begin
                  if Ucl.type_is_string (item) then
                     return True;
                  else
                     generic_error_emission ("a string");
                     return False;
                  end if;
               end content_is_string_type;

               function content_is_integer_type return Boolean is
               begin
                  if Ucl.type_is_integer (item) then
                     return True;
                  else
                     generic_error_emission ("an integer");
                     return False;
                  end if;
               end content_is_integer_type;

               function content_is_object_type return Boolean is
               begin
                  if Ucl.type_is_object (item) then
                     return True;
                  else
                     generic_error_emission ("an object");
                     return False;
                  end if;
               end content_is_object_type;

            begin
               if IsBlank (ukey) then
                  null;
               elsif ukey = "URL" then
                  if content_is_string_type then
                     url := SUS (Ucl.ucl_object_tostring (item));
                  else
                     return;
                  end if;
               elsif ukey = "PUBKEY" then
                  if content_is_string_type then
                     pubkey := SUS (Ucl.ucl_object_tostring (item));
                  else
                     return;
                  end if;
               elsif ukey = "MIRROR_TYPE" then
                  if content_is_string_type then
                     mirror_type := SUS (Ucl.ucl_object_tostring (item));
                  else
                     return;
                  end if;
               elsif ukey = "SIGNATURE_TYPE" then
                  if content_is_string_type then
                     sig_type := SUS (Ucl.ucl_object_tostring (item));
                  else
                     return;
                  end if;
               elsif ukey = "FINGERPRINTS" then
                  if content_is_string_type then
                     fingerprints := SUS (Ucl.ucl_object_tostring (item));
                  else
                     return;
                  end if;
               elsif ukey = "IP_VERSION" then
                  if content_is_integer_type then
                     use_ipvx := Ucl.ucl_object_toint (item);
                     case use_ipvx is
                        when 4 | 6 => null;
                        when others => use_ipvx := 0;
                     end case;
                  else
                     return;
                  end if;
               elsif ukey = "PRIORITY" then
                  if content_is_integer_type then
                     priority := Ucl.ucl_object_toint (item);
                  else
                     return;
                  end if;
               elsif ukey = "ENV" then
                  if content_is_object_type then
                     env_obj := item;
                  else
                     return;
                  end if;
               end if;
            end;
         end loop;

         new_repo.name         := reponame_txt;
         new_repo.enable       := enable_repo;
         new_repo.url          := url;
         new_repo.pubkey       := pubkey;
         new_repo.fingerprints := fingerprints;

         if equivalent (uppercase (mirror_type), "SRV") then
            new_repo.mirror_type := SRV;
         elsif equivalent (uppercase (mirror_type), "HTTP") then
            new_repo.mirror_type := HTTP;
         else
            new_repo.mirror_type := NOMIRROR;
         end if;

         if equivalent (uppercase (sig_type), "PUBKEY") then
            new_repo.signature_type := SIG_PUBKEY;
         elsif equivalent (uppercase (sig_type), "FINGERPRINTS") then
            new_repo.signature_type := SIG_FINGERPRINT;
         else
            new_repo.signature_type := SIG_NONE;
         end if;

         --  Priority for protocol: 1) flags, 2) config("IP_VERSION"), 3) repository("IP_VERSION")
         if flags = init_none then
            declare
               proto : int64 := Config.configuration_value (config.ip_version);
            begin
               if proto = 0 then
                  case use_ipvx is
                     when 4 => new_repo.flags := REPO_FLAGS_LIMIT_IPV4;
                     when 6 => new_repo.flags := REPO_FLAGS_LIMIT_IPV6;
                     when others => new_repo.flags := REPO_FLAGS_DEFAULT;
                  end case;
               else
                  case proto is
                     when 4 => new_repo.flags := REPO_FLAGS_LIMIT_IPV4;
                     when 6 => new_repo.flags := REPO_FLAGS_LIMIT_IPV6;
                     when others => new_repo.flags := REPO_FLAGS_DEFAULT;
                  end case;
               end if;
            end;
         else
            if flags = init_use_ipv4 then
               new_repo.flags := REPO_FLAGS_LIMIT_IPV4;
            else
               new_repo.flags := REPO_FLAGS_LIMIT_IPV6;
            end if;
         end if;

         if priority < int64 (A_priority'First) then
            new_repo.priority := A_priority (A_priority'First);
         elsif priority > int64 (A_priority'Last) then
            new_repo.priority := A_priority (A_priority'Last);
         else
            new_repo.priority := A_priority (priority);
         end if;


         if env_obj /= null then
            declare
               iter : aliased libucl.ucl_object_iter_t :=
                              libucl.ucl_object_iter_t (System.Null_Address);
               item : access constant libucl.ucl_object_t;
            begin
               loop
                  item := Ucl.ucl_object_iterate (env_obj, iter'Access, True);
                  exit when item = null;

                  declare
                     keytxt : Text := SUS (Ucl.ucl_object_key (item));
                     valtxt : Text := SUS (Ucl.ucl_object_tostring_forced (item));
                  begin
                     if new_repo.env.Contains (keytxt) then
                        new_repo.env.Replace (keytxt, valtxt);
                     else
                        new_repo.env.Insert (keytxt, valtxt);
                     end if;
                  end;
               end loop;
            end;
         end if;
      end;

      if found_repo then
         repositories.Replace (reponame_txt, new_repo);
      else
         repositories.Insert (reponame_txt, new_repo);
      end if;
   end add_repo;


   --------------------------------------------------------------------
   --  load_repositories
   --------------------------------------------------------------------
   procedure load_repositories (repodirs : String; flags : Init_protocol)
   is
      num_dirs : Natural;
   begin
      if IsBlank (repodirs) then
         return;
      end if;

      num_dirs := count_char (repodirs, LAT.LF) + 1;

      for D in 1 .. num_dirs loop
         declare
            repodir : constant String := specific_field (repodirs, D, LAT.LF & "");
         begin
            load_repo_files (repodir, flags);
         end;
      end loop;
   end load_repositories;

end Core.Repo.Read;
