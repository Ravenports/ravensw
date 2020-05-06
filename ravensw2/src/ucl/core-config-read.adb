--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Latin_1;
with System;

with Core.Strings;
with Core.Context;
with Core.Object;
with Ucl;


package body Core.Config.Read is

   package LAT renames Ada.Characters.Latin_1;
   package COB renames Core.Object;
   package CS  renames Core.Strings;

   --------------------------------------------------------------------
   --  config_get
   --------------------------------------------------------------------
   function config_get (key : String) return access constant libucl.ucl_object_t is
   begin
      return Ucl.ucl_object_find_key (config_object, key);
   end config_get;


   --------------------------------------------------------------------
   --  config_get_string
   --------------------------------------------------------------------
   function config_get_string (key : String) return String is
   begin
      return COB.pkg_object_string (config_get (key));
   end config_get_string;


   --------------------------------------------------------------------
   --  config_get_boolean
   --------------------------------------------------------------------
   function config_get_boolean (key : String) return Boolean is
   begin
      return COB.pkg_object_bool (config_get (key));
   end config_get_boolean;


   --------------------------------------------------------------------
   --  config_get_int64
   --------------------------------------------------------------------
   function config_get_int64 (key : String) return int64 is
   begin
      return COB.pkg_object_int (config_get (key));
   end config_get_int64;


   --------------------------------------------------------------------
   --  config_dump
   --------------------------------------------------------------------
   function config_dump return String is
   begin
      return Object.pkg_object_dump (config_object);
   end config_dump;


   --------------------------------------------------------------------
   --  ucl_configuration_value #1 (Boolean result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return Boolean is
   begin
      if config_get_type (ci) = pkg_bool then
         return config_get_boolean (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


   --------------------------------------------------------------------
   --  ucl_configuration_value #2 (int64 result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return int64 is
   begin
      if config_get_type (ci) = pkg_int then
         return config_get_int64 (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


   --------------------------------------------------------------------
   --  ucl_configuration_value #3 (string result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return String is
   begin
      if config_get_type (ci) = pkg_string then
         return config_get_string (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


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
   --  convert_string_to_ucl
   --------------------------------------------------------------------
   function convert_string_to_ucl_object (cetype  : Config_Entry_Type;
                                          payload : String) return access libucl.ucl_object_t
   is
      obj          : access libucl.ucl_object_t;
      inserted     : Boolean;
      rootdir_used : constant Boolean := not IsBlank (Context.reveal_pkg_rootdir);
   begin
      case cetype is
         when pkg_string =>
            declare
               tmp     : Text;
               tmp_set : Boolean := False;
            begin
               if rootdir_used and then leads (payload, "/")
               then
                  SU.Append (tmp, Context.reveal_pkg_rootdir);
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
   --  load_repo_file
   --------------------------------------------------------------------
   procedure load_repo_file (dfd      : Unix.File_Descriptor;
                             repodir  : String;
                             repofile : String;
                             flags    : Pkg_init_flags)
   is
      parser  : Ucl.T_parser;
      obj     : access libucl.ucl_object_t;
      key_ABI : constant String := get_ci_key (abi);
      fd      : Unix.File_Descriptor;
      success : Boolean;
      res     : Boolean;
   begin
      parser := Ucl.ucl_parser_new_basic;
      declare
         myarch : String := config_get_string (key_ABI);
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


end Core.Config.Read;
