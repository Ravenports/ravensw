--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Containers.Vectors;
with Core.Event;
with Core.Unix;
with Core.Strings;
with Core.Object;
with Core.Metalog;
with Core.Pkg;    use Core.Pkg;
with System;

package body Core.Config is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package ENV renames Ada.Environment_Variables;
   package CON renames Ada.Containers;
   package EV  renames Core.Event;
   package CS  renames Core.Strings;
   package COB renames Core.Object;

   --------------------------------------------------------------------
   --  pkg_initialized
   --------------------------------------------------------------------
   function pkg_initialized return Boolean is
   begin
      return parsed;
   end pkg_initialized;


   --------------------------------------------------------------------
   --  pkg_init
   --------------------------------------------------------------------
   function pkg_init
     (path     : String;
      reposdir : String;
      dlevel   : ST_Debug_Level) return Pkg_Error_Type is
   begin
      return pkg_ini (path, reposdir, init_none, dlevel);
   end pkg_init;


   --------------------------------------------------------------------
   --  pkg_config_get
   --------------------------------------------------------------------
   function pkg_config_get (key : String) return access constant libucl.ucl_object_t is
   begin
      return Ucl.ucl_object_find_key (config_object, key);
   end pkg_config_get;


   --------------------------------------------------------------------
   --  pkg_config_get_string
   --------------------------------------------------------------------
   function pkg_config_get_string (key : String) return String is
   begin
      return COB.pkg_object_string (pkg_config_get (key));
   end pkg_config_get_string;


   --------------------------------------------------------------------
   --  pkg_config_get_boolean
   --------------------------------------------------------------------
   function pkg_config_get_boolean (key : String) return Boolean is
   begin
      return COB.pkg_object_bool (pkg_config_get (key));
   end pkg_config_get_boolean;


   --------------------------------------------------------------------
   --  pkg_config_get_int64
   --------------------------------------------------------------------
   function pkg_config_get_int64 (key : String) return Ucl.int64 is
   begin
      return COB.pkg_object_int (pkg_config_get (key));
   end pkg_config_get_int64;


   --------------------------------------------------------------------
   --  pkg_ini
   --------------------------------------------------------------------
   function pkg_ini
     (path     : String;
      reposdir : String;
      flags    : Pkg_init_flags;
      dlevel   : ST_Debug_Level) return Pkg_Error_Type
   is
      conffd       : Unix.File_Descriptor;
      default_conf : constant String := rel_prefix & "/etc/ravensw.conf";
      open_rdonly  : constant Unix.T_Open_Flags := (RDONLY => True, others => False);
      parser       : Ucl.T_parser;
      fatal_errors : Boolean := False;
   begin
      if not Unix.file_connected (context.rootfd) then
         declare
            flags : Unix.T_Open_Flags;
         begin
            flags.DIRECTORY := True;
            flags.RDONLY    := True;
            flags.CLOEXEC   := True;
            context.rootfd := Unix.open_file ("/", flags);
            if not Unix.file_connected (context.rootfd) then
               EV.pkg_emit_error (SUS ("Impossible to open /"));
               return EPKG_FATAL;
            end if;
         end;
      end if;

      --  TODO: pkg_get_myarch(myabi, BUFSIZ, &ctx.osversion);
      --        pkg_get_myarch_legacy(myabi_legacy, BUFSIZ);

      if parsed then
         EV.pkg_emit_error (SUS ("pkg_init() must only be called once"));
         return EPKG_FATAL;
      end if;

      config_object := Ucl.ucl_object_typed_new_object;

      declare
         obj      : access libucl.ucl_object_t;
         inserted : Boolean;
      begin
         for index in config_entries'Range loop
            obj := convert_string_to_ucl_object (cetype  => config_entries (index).config_type,
                                                 payload => USS (config_entries (index).default));

            inserted := Ucl.ucl_object_insert_key
              (top      => config_object,
               elt      => obj,
               key      => USS (config_entries (index).key),
               copy_key => False);
         end loop;
      end;

      if path = "" then
         if DIR.Exists ("/" & default_conf) then
            conffd := Unix.open_file (dirfd         => context.rootfd,
                                      relative_path => default_conf,
                                      flags         => open_rdonly);
            if not Unix.file_connected (conffd) then
               EV.pkg_emit_with_strerror (SUS ("Cannot open /" & default_conf));
            end if;
         end if;
      else
         if DIR.Exists (path) then
            conffd := Unix.open_file (path, open_rdonly);
            if not Unix.file_connected (conffd) then
               EV.pkg_emit_with_strerror (SUS ("Cannot open " & path));
            end if;
         end if;
      end if;

      parser := Ucl.ucl_parser_new_basic;
      --  TODO: ucl_parser_register_variable (p, "ABI", myabi);
      --  TODO: ucl_parser_register_variable (p, "ALTABI", myabi_legacy);

      Unix.reset_errno;
      declare
         obj      : access libucl.ucl_object_t;
         conitem  : access constant libucl.ucl_object_t;
         item     : access constant libucl.ucl_object_t;
         ncfg     : access libucl.ucl_object_t;
         iter     : aliased libucl.ucl_object_iter_t :=
                            libucl.ucl_object_iter_t (System.Null_Address);
         success  : Boolean;
         inserted : Boolean;
         virgin   : Boolean := True;
      begin
         if Unix.file_connected (conffd) then
            if not Ucl.ucl_parser_add_fd (parser, conffd) then
               EV.pkg_emit_error (SUS ("Invalid configuration file: " &
                                    Ucl.ucl_parser_get_error (parser)));
            else
               obj := Ucl.ucl_parser_get_object (parser);
            end if;
            success := Unix.close_file (conffd);

            loop
               item := Ucl.ucl_object_iterate (obj, iter'Access, True);
               exit when item = null;

               declare
                  key        : constant String := Ucl.ucl_object_key (item);
                  ukey       : constant String := CS.uppercase (key);
               begin
                  if ukey = "PACKAGESITE" or else
                    ukey = "PUBKEY" or else
                    ukey = "MIRROR_TYPE"
                  then
                     EV.pkg_emit_error
                       (SUS (ukey & " in ravensw.conf is no longer supported.  " &
                          "Convert to the new repository style.  See ravensw.conf(5)"));
                     fatal_errors := True;
                  else
                     conitem := Ucl.ucl_object_keyl (config_object, ukey);

                     --  ignore unknown keys
                     if conitem /= null then
                        if not Ucl.object_types_equal (conitem, item) then
                           EV.pkg_emit_error (SUS ("Malformed key " & key & ", ignoring"));
                        else
                           if virgin then
                              virgin := False;
                              ncfg := Ucl.ucl_object_typed_new_object;
                           end if;

                           inserted :=
                             Ucl.ucl_object_insert_key
                               (top      => ncfg,
                                elt      => libucl.ucl_object_copy (item),
                                key      => ukey,
                                copy_key => True);
                        end if;
                     end if;
                  end if;
               end;
            end loop;

            libucl.ucl_object_unref (obj);
            libucl.ucl_parser_free (parser);

            if fatal_errors then
               libucl.ucl_object_unref (ncfg);
               return EPKG_FATAL;
            end if;

            if not virgin then
               iter := libucl.ucl_object_iter_t (System.Null_Address);
               loop
                  item := Ucl.ucl_object_iterate (ncfg, iter'Access, True);
                  exit when item = null;

                  declare
                     key : constant String := Ucl.ucl_object_key (item);
                  begin
                     inserted :=
                       Ucl.ucl_object_replace_key (top      => config_object,
                                                   elt      => libucl.ucl_object_ref (item),
                                                   key      => key,
                                                   copy_key => True);
                  end;
                  libucl.ucl_object_unref (ncfg);
               end loop;
            end if;

         end if;
      end;

      declare
         ncfg     : access libucl.ucl_object_t;
         iter     : aliased libucl.ucl_object_iter_t :=
                            libucl.ucl_object_iter_t (System.Null_Address);
         obj      : access libucl.ucl_object_t;
         item     : access constant libucl.ucl_object_t;
         inserted : Boolean;
         virgin   : Boolean := True;
      begin
         loop
            item := Ucl.ucl_object_iterate (config_object, iter'Access, True);
            exit when item = null;

            declare
               key : String := Ucl.ucl_object_key (item);
            begin
               declare
                  val     : String := ENV.Value (key);
                  contype : Config_Entry_Type := convert (item);
               begin

                  obj := convert_string_to_ucl_object (contype, val);

                  if virgin then
                     virgin := False;
                     ncfg := Ucl.ucl_object_typed_new_object;
                  end if;

                  inserted :=
                    Ucl.ucl_object_insert_key
                      (top      => ncfg,
                       elt      => obj,
                       key      => key,
                       copy_key => True);
               end;
            exception
               when Constraint_Error => null;   -- no env override, do nothing
               when Unsupported_Type =>
                  EV.pkg_emit_error (SUS ("pkg_ini: unsupported type: " & item.c_type'Img));
            end;
         end loop;

         if not virgin then
            iter := libucl.ucl_object_iter_t (System.Null_Address);
            loop
               item := Ucl.ucl_object_iterate (ncfg, iter'Access, True);
               exit when item = null;

               declare
                  key : constant String := Ucl.ucl_object_key (item);
               begin
                  inserted :=
                    Ucl.ucl_object_replace_key (top      => config_object,
                                                elt      => libucl.ucl_object_ref (item),
                                                key      => key,
                                                copy_key => True);
               end;
            end loop;
            libucl.ucl_object_unref (ncfg);
         end if;
      end;

      parsed := True;

      if dlevel > 0 then
         --  Let command line option override conf
         context.debug_level := dlevel;
      else
         declare
            use type Ucl.int64;
            DL : Ucl.int64 := pkg_config_get_int64 ("DEBUG_LEVEL");
         begin
            if DL >= Ucl.int64 (ST_Debug_Level'First) and then
              DL <= Ucl.int64 (ST_Debug_Level'Last)
            then
               context.debug_level := ST_Debug_Level (DL);
            else
               EV.pkg_emit_notice (SUS ("DEBUG_LEVEL out of range, ignoring"));
            end if;
         end;
      end if;

      --  TODO: check ABI isn't "unknown"

      EV.pkg_debug (1, "ravensw initialized");

      --  Start the event pipe
      --  If used, event pipe closed by Core.Finalize.cleanup
      declare
         evpipe : String := pkg_config_get_string ("EVENT_PIPE");
      begin
         if not IsBlank (evpipe) then
            connect_evpipe (evpipe);
         end if;
      end;

      context.developer_mode := pkg_config_get_boolean ("DEVELOPER_MODE");

      declare
         useragent : String := pkg_config_get_string ("HTTP_USER_AGENT");
         variable  : String := "HTTP_USER_AGENT";
      begin
         if IsBlank (useragent) then
            ENV.Set (variable, "ravensw/" & progversion);
         else
            ENV.Set (variable, useragent);
         end if;
      end;

      declare
         obj      : access constant libucl.ucl_object_t;
         iter     : aliased libucl.ucl_object_iter_t :=
                            libucl.ucl_object_iter_t (System.Null_Address);
         item     : access constant libucl.ucl_object_t;
      begin
         obj := pkg_config_get ("PKG_ENV");
         loop
            item := Ucl.ucl_object_iterate (config_object, iter'Access, True);
            exit when item = null;

            declare
               key     : String := Ucl.ucl_object_key (item);
               payload : String := Ucl.ucl_object_tostring_forced (item);
            begin
               EV.pkg_debug (1, "Setting env var: " & key);
               if not IsBlank (key) then
                  ENV.Set (key, payload);
               end if;
            end;
         end loop;
      end;

      load_repositories (reposdir, flags);

      --  TODO: validate the different scheme
      --  TODO: bypass resolv.conf

      declare
         metalog_file : String := pkg_config_get_string ("METALOG");
      begin
         if not IsBlank (metalog_file) then
            if Metalog.metalog_open (metalog_file) /= EPKG_OK then
               return EPKG_FATAL;
            end if;
         end if;
      end;

      return EPKG_OK;
   end pkg_ini;


   --------------------------------------------------------------------
   --  connect_evpipe
   --------------------------------------------------------------------
   procedure connect_evpipe (event_pipe : String)
   is
      file_exists : Boolean;
      mechanism   : Unix.Unix_Pipe;
      sock_error  : Text := SUS ("open event pipe (socket)");
      sock_flags  : Unix.T_Open_Flags;
   begin
      begin
         file_exists := DIR.Exists (Name => event_pipe);
      exception
         when others =>
            file_exists := False;
      end;

      if not file_exists then
         EV.pkg_emit_error (SUS ("No such event pipe: " & event_pipe));
         return;
      end if;

      case DIR.Kind (Name => event_pipe) is
         when DIR.Ordinary_File =>
            EV.pkg_emit_error (SUS (event_pipe & " is an ordinary file"));
            return;
         when DIR.Directory =>
            EV.pkg_emit_error (SUS (event_pipe & " is a directory"));
            return;
         when DIR.Special_File =>
            null;
      end case;

      mechanism := Unix.IPC_mechanism (event_pipe);
      case mechanism is

         when Unix.something_else =>
            EV.pkg_emit_error (SUS (event_pipe & " is not a fifo or socket"));

         when Unix.named_pipe =>
            sock_flags.WRONLY := True;
            sock_flags.NON_BLOCK := True;
            context.eventpipe := Unix.open_file (event_pipe, sock_flags);
            if not Unix.file_connected (context.eventpipe) then
               EV.pkg_emit_errno (SUS ("open event pipe (FIFO)"), SUS (event_pipe), Unix.errno);
            end if;

         when Unix.unix_socket =>
            declare
               fd : Unix.File_Descriptor;
            begin
               case Unix.connect_unix_socket (event_pipe, fd) is
                  when Unix.connected =>
                     context.eventpipe := fd;

                  when Unix.failed_creation | Unix.failed_connection =>
                     EV.pkg_emit_errno (sock_error, SUS (event_pipe), Unix.errno);

                  when Unix.failed_population =>
                     EV.pkg_emit_error (SUS ("Socket path too long: " & event_pipe));
               end case;
            end;
      end case;

   end connect_evpipe;


   --------------------------------------------------------------------
   --  convert_string_to_ucl
   --------------------------------------------------------------------
   function convert_string_to_ucl_object (cetype  : Config_Entry_Type;
                                          payload : String) return access libucl.ucl_object_t
   is
      obj          : access libucl.ucl_object_t;
      inserted     : Boolean;
      rootdir_used : constant Boolean := not IsBlank (context.pkg_rootdir);
   begin
      case cetype is
         when pkg_string =>
            declare
               tmp     : Text;
               tmp_set : Boolean := False;
            begin
               if rootdir_used and then leads (payload, "/")
               then
                  SU.Append (tmp, context.pkg_rootdir);
                  SU.Append (tmp, payload);
                  tmp_set := True;
               end if;
               if tmp_set then
                  obj := Ucl.ucl_object_fromstring_and_trim (USS (tmp));
               else
                  obj := Ucl.ucl_object_fromstring_and_trim (payload);
               end if;
            end;
         when pkg_bool =>
            obj := Ucl.ucl_object_fromstring_boolean (payload);
         when pkg_int =>
            obj := Ucl.ucl_object_fromstring_int (payload);
         when pkg_array =>
            obj := Ucl.ucl_object_typed_new_array;
            --  format A,B,C,D
            if not IsBlank (payload) then
               declare
                  nf  : Natural := CS.count_char (payload, LAT.Comma);
               begin
                  for k in 1 .. nf + 1 loop
                     inserted := Ucl.ucl_array_push (obj,
                                                     Ucl.ucl_object_fromstring_and_trim
                                                       (CS.specific_field (payload, k, ",")));
                  end loop;
               end;
            end if;
         when pkg_object =>
            obj := Ucl.ucl_object_typed_new_object;
            --  format A=B,C=D,E=F,G=H
            if not IsBlank (payload) then
               declare
                  nf  : Natural := CS.count_char (payload, LAT.Comma);
               begin
                  for k in 1 .. nf + 1 loop
                     declare
                        nvpair : constant String := CS.specific_field (payload, k, ",");
                     begin
                        if CS.contains (nvpair, "=") then
                           inserted := Ucl.ucl_object_insert_key
                             (top      => obj,
                              elt      =>
                                Ucl.ucl_object_fromstring_and_trim (CS.part_2 (nvpair)),
                              key      => CS.part_1 (nvpair),
                              copy_key => False);
                        end if;
                     end;
                  end loop;
               end;
            end if;
      end case;
      return obj;
   end convert_string_to_ucl_object;


   --------------------------------------------------------------------
   --  convert
   --------------------------------------------------------------------
   function convert (obj : access constant libucl.ucl_object_t) return Config_Entry_Type
   is
      ut : libucl.ucl_type_t := libucl.ucl_object_type (obj);
   begin
      case ut is
         when libucl.UCL_OBJECT  => return pkg_object;
         when libucl.UCL_ARRAY   => return pkg_array;
         when libucl.UCL_INT     => return pkg_int;
         when libucl.UCL_STRING  => return pkg_string;
         when libucl.UCL_BOOLEAN => return pkg_bool;
         when libucl.UCL_FLOAT |
              libucl.UCL_TIME |
              libucl.UCL_USERDATA |
              libucl.UCL_NULL =>
            raise Unsupported_Type with ut'Img;
      end case;
   end convert;


   --------------------------------------------------------------------
   --  pkg_config_dump
   --------------------------------------------------------------------
   function pkg_config_dump return String is
   begin
      return Object.pkg_object_dump (config_object);
   end pkg_config_dump;


   --------------------------------------------------------------------
   --  load_repo_file
   --------------------------------------------------------------------
   procedure load_repo_file (dfd      : Unix.File_Descriptor;
                             repodir  : String;
                             repofile : String;
                             flags    : Pkg_init_flags)
   is
      parser  : Ucl.T_parser;
      obj     : access libucl.ucl_object_t;
      key_ABI : constant String := "ABI";
      fd      : Unix.File_Descriptor;
      success : Boolean;
      res     : Boolean;
   begin
      parser := Ucl.ucl_parser_new_basic;
      declare
         myarch : String := pkg_config_get_string (key_ABI);
      begin
         Ucl.ucl_parser_register_variable (parser, key_ABI, myarch);
      end;
      EV.pkg_debug (1, "PkgConfig: loading " & repodir & "/" & repofile);
      fd := Unix.open_file (dfd, repofile, (RDONLY => True, others => False));

      if not Unix.file_connected (fd) then
         EV.pkg_emit_with_strerror (SUS ("Unable to open '" & repodir & "/" & repofile & "'"));
         return;
      end if;

      success := Ucl.ucl_parser_add_fd (parser, fd);
      res := Unix.close_file (fd);

      if not success then
         EV.pkg_emit_with_strerror (SUS ("Error parsing: '" & repodir & "/" & repofile & "'"));
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
   procedure load_repo_files (repodir : String; flags : Pkg_init_flags)
   is
      procedure loadfile (position : text_crate.Cursor);
      procedure populate_priority (position : pkg_repos_crate.Cursor);

      fd : Unix.File_Descriptor;
      res : Boolean;

      procedure loadfile (position : text_crate.Cursor)
      is
         filename : String := USS (text_crate.Element (position));
      begin
         load_repo_file (dfd      => fd,
                         repodir  => repodir,
                         repofile => filename,
                         flags    => flags);
      end loadfile;

      procedure populate_priority (position : pkg_repos_crate.Cursor)
      is
         item    : T_pkg_repo renames pkg_repos_crate.Element (position);
         priorec : T_repo_priority;
      begin
         priorec.reponame := item.name;
         priorec.priority := item.priority;
         repositories_order.Append (priorec);
      end populate_priority;

   begin
      EV.pkg_debug (1, "PkgConfig: loading repositories in " & repodir);

      --  Don't bother to load in alphabetical order like pkg(8) does
      --  1) it loads into a hashed map which is a random order anyway
      --  2) pkg(8) has a bug where it ignores priority order.  We're going to fix
      --  that and sort by priority after loading.

      declare
         Search    : DIR.Search_Type;
         Dirent    : DIR.Directory_Entry_Type;
         tempstore : text_crate.Vector;
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
   --  load_repositories
   --------------------------------------------------------------------
   procedure load_repositories (repodir : String; flags : Pkg_init_flags) is
   begin
      if not IsBlank (repodir) then
         load_repo_files (repodir, flags);
      else
         declare
            iter : aliased libucl.ucl_object_iter_t :=
                           libucl.ucl_object_iter_t (System.Null_Address);
            item : access constant libucl.ucl_object_t;
            reposlist : access constant libucl.ucl_object_t := pkg_config_get ("REPOS_DIR");
         begin
            loop
               item := Ucl.ucl_object_iterate (reposlist, iter'Access, True);
               exit when item = null;

               declare
                  diritem : String := COB.pkg_object_string (item);
               begin
                  load_repo_files (diritem, flags);
               end;
            end loop;
         end;
      end if;
   end load_repositories;


   --------------------------------------------------------------------
   --  walk_repo_obj
   --------------------------------------------------------------------
   procedure walk_repo_obj (fileobj  : access constant libucl.ucl_object_t;
                            filename : String;
                            flags    : Pkg_init_flags)
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
            EV.pkg_debug (1, "PkgConfig: parsing key '" & key & "'");
            if repositories.Contains (keystr) then
               EV.pkg_debug (1, "PkgConfig: overwriting repository " & key);
            end if;
            if Ucl.type_is_object (item) then
               add_repo (repo_obj => item, reponame => key, flags => flags);
            else
               EV.pkg_emit_error (SUS ("Ignoring bad configuration entry in " &
                                    filename & ": " & Ucl.ucl_emit_yaml (item)));
            end if;
         end;
      end loop;
   end walk_repo_obj;


   --------------------------------------------------------------------
   --  add_repo
   --------------------------------------------------------------------
   procedure add_repo (repo_obj : access constant libucl.ucl_object_t;
                       reponame : String;
                       flags    : Pkg_init_flags)
   is
      enabled      : access constant libucl.ucl_object_t;
      reponame_txt : Text := SUS (reponame);
      new_repo     : T_pkg_repo;
      found_repo   : Boolean;
      enable_repo  : Boolean;
   begin
      EV.pkg_debug (1, "PkgConfig: parsing repository object " & reponame);

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
            EV.pkg_debug (1, "PkgConfig: disabling repo " & reponame);

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
         use_ipvx     : Ucl.int64;
         priority     : Ucl.int64;
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
                  EV.pkg_emit_error (SUS ("Expecting " & flavor & " for the '" & key &
                                       "' key of the '" & reponame & "' repo"));
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
               use type Ucl.int64;
               proto : Ucl.int64 := pkg_config_get_int64 ("IP_VERSION");
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

         declare
            use type Ucl.int64;
         begin
            if priority < Ucl.int64 (T_priority'First) then
               new_repo.priority := T_priority (T_priority'First);
            elsif priority > Ucl.int64 (T_priority'Last) then
               new_repo.priority := T_priority (T_priority'Last);
            else
               new_repo.priority := T_priority (priority);
            end if;
         end;


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
   --  pkg_repo_url
   --------------------------------------------------------------------
   function pkg_repo_url (repo : T_pkg_repo) return String is
   begin
      return USS (repo.url);
   end pkg_repo_url;


   --------------------------------------------------------------------
   --  pkg_repo_name
   --------------------------------------------------------------------
   function pkg_repo_name (repo : T_pkg_repo) return String is
   begin
      return USS (repo.name);
   end pkg_repo_name;


   --------------------------------------------------------------------
   --  pkg_repo_pubkey
   --------------------------------------------------------------------
   function pkg_repo_pubkey (repo : T_pkg_repo) return String is
   begin
      return USS (repo.pubkey);
   end pkg_repo_pubkey;


   --------------------------------------------------------------------
   --  pkg_repo_fingerprints
   --------------------------------------------------------------------
   function pkg_repo_fingerprints (repo : T_pkg_repo) return String is
   begin
      return USS (repo.fingerprints);
   end pkg_repo_fingerprints;


   --------------------------------------------------------------------
   --  pkg_repo_enabled #1
   --------------------------------------------------------------------
   function pkg_repo_enabled (repo : T_pkg_repo) return String is
   begin
      case repo.enable is
         when False => return "no";
         when True  => return "yes";
      end case;
   end pkg_repo_enabled;


   --------------------------------------------------------------------
   --  pkg_repo_enabled #2
   --------------------------------------------------------------------
   function pkg_repo_enabled (repo : T_pkg_repo) return Boolean is
   begin
      return repo.enable;
   end pkg_repo_enabled;


   --------------------------------------------------------------------
   --  pkg_repo_mirror_type #1
   --------------------------------------------------------------------
   function pkg_repo_mirror_type (repo : T_pkg_repo) return String is
   begin
      case repo.mirror_type is
         when SRV      => return "SRV";
         when HTTP     => return "HTTP";
         when NOMIRROR => return "NONE";
      end case;
   end pkg_repo_mirror_type;


   --------------------------------------------------------------------
   --  pkg_repo_mirror_type #2
   --------------------------------------------------------------------
   function pkg_repo_mirror_type (repo : T_pkg_repo) return T_mirror_type is
   begin
      return repo.mirror_type;
   end pkg_repo_mirror_type;


   --------------------------------------------------------------------
   --  pkg_repo_signature_type #1
   --------------------------------------------------------------------
   function pkg_repo_signature_type (repo : T_pkg_repo) return String is
   begin
      case repo.signature_type is
         when SIG_PUBKEY      => return "PUBKEY";
         when SIG_FINGERPRINT => return "FINGERPRINTS";
         when SIG_NONE        => return "NONE";
      end case;
   end pkg_repo_signature_type;


   --------------------------------------------------------------------
   --  pkg_repo_signature_type #2
   --------------------------------------------------------------------
   function pkg_repo_signature_type (repo : T_pkg_repo) return T_signature is
   begin
      return repo.signature_type;
   end pkg_repo_signature_type;


   --------------------------------------------------------------------
   --  pkg_repo_priority_type #1
   --------------------------------------------------------------------
   function pkg_repo_priority_type (repo : T_pkg_repo) return String is
   begin
      return int2str (Integer (repo.priority));
   end pkg_repo_priority_type;


   --------------------------------------------------------------------
   --  pkg_repo_priority_type #2
   --------------------------------------------------------------------
   function pkg_repo_priority_type (repo : T_pkg_repo) return T_priority is
   begin
      return repo.priority;
   end pkg_repo_priority_type;


   --------------------------------------------------------------------
   --  pkg_repo_ipv_type #1
   --------------------------------------------------------------------
   function pkg_repo_ipv_type (repo : T_pkg_repo) return String is
   begin
      case repo.flags is
         when REPO_FLAGS_LIMIT_IPV4 => return "4";
         when REPO_FLAGS_LIMIT_IPV6 => return "6";
         when REPO_FLAGS_DEFAULT    => return "0";
      end case;
   end pkg_repo_ipv_type;


   --------------------------------------------------------------------
   --  pkg_repo_ipv_type #2
   --------------------------------------------------------------------
   function pkg_repo_ipv_type (repo : T_pkg_repo) return T_pkg_repo_flags is
   begin
      return repo.flags;
   end pkg_repo_ipv_type;

end Core.Config;
