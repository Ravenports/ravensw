--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Environment_Variables;
with Ada.Characters.Latin_1;
with Ada.Directories;
with System;

with Core.Strings;
with Core.Context;
with Core.Object;
with Core.Unix;
with Core.Event;
with Core.Elf_Operations;
with Core.Repo;
with Core.Metalog;
with Ucl;


package body Core.Config.Read is

   package LAT renames Ada.Characters.Latin_1;
   package ENV renames Ada.Environment_Variables;
   package DIR renames Ada.Directories;
   package COB renames Core.Object;
   package CS  renames Core.Strings;
   package EV  renames Core.Event;


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
   --  connect_evpipe
   --------------------------------------------------------------------
   procedure connect_evpipe (event_pipe : String)
   is
      file_exists : Boolean;
      mechanism   : Unix.Unix_Pipe;
      sock_error  : constant String := "open event pipe (socket)";
   begin
      begin
         file_exists := DIR.Exists (Name => event_pipe);
      exception
         when others =>
            file_exists := False;
      end;

      if not file_exists then
         EV.emit_error ("No such event pipe: " & event_pipe);
         return;
      end if;

      case DIR.Kind (Name => event_pipe) is
         when DIR.Ordinary_File =>
            EV.emit_error (event_pipe & " is an ordinary file");
            return;
         when DIR.Directory =>
            EV.emit_error (event_pipe & " is a directory");
            return;
         when DIR.Special_File =>
            null;
      end case;

      mechanism := Unix.IPC_mechanism (event_pipe);
      case mechanism is

         when Unix.something_else =>
            EV.emit_error (event_pipe & " is not a fifo or socket");

         when Unix.named_pipe =>
            if not Context.register_event_pipe_via_file (event_pipe) then
               EV.emit_errno ("open event pipe (FIFO)", event_pipe, Unix.errno);
            end if;

         when Unix.unix_socket =>
            case Context.register_event_pipe_via_socket (event_pipe) is
               when Unix.connected =>
                  null;
               when Unix.failed_creation
                  | Unix.failed_connection =>
                  EV.emit_errno (sock_error, event_pipe, Unix.errno);
               when Unix.failed_population =>
                  EV.emit_error ("Socket path too long: " & event_pipe);
            end case;
      end case;

   end connect_evpipe;


   --------------------------------------------------------------------
   --  establish_configuration
   --------------------------------------------------------------------
   function establish_configuration
     (path     : String;
      reposdir : String;
      flags    : Init_protocol;
      dlevel   : ST_Debug_Level;
      options  : String) return Action_Result
   is
      conffd       : Unix.File_Descriptor;
      default_conf : constant String := rel_prefix & "/etc/ravensw.conf";
      open_rdonly  : constant Unix.T_Open_Flags := (RDONLY => True, others => False);
      parser       : Ucl.T_parser;
      fatal_errors : Boolean := False;
      abicalc      : Elf_Operations.abi_result;
   begin
      if parsed then
         EV.emit_error ("pkg_init() must only be called once");
         return RESULT_FATAL;
      end if;

      if not Unix.file_connected (Context.reveal_root_fd) then
         EV.emit_error ("Impossible to open /");
         return RESULT_FATAL;
      end if;

      abicalc := Elf_Operations.calculate_abi;
      if abicalc.error /= RESULT_OK then
         return abicalc.error;
      end if;

      --  Build config object with default values
      config_object := Ucl.ucl_object_typed_new_object;
      for ci in Configuration_Item'Range loop
         declare
            obj      : access libucl.ucl_object_t;
            inserted : Boolean;
         begin
            case ci is
               when abi =>
                  obj := convert_string_to_ucl_object (config_get_type (ci),
                                                       USS (abicalc.abi));
               when others =>
                  obj := convert_string_to_ucl_object (config_get_type (ci),
                                                       config_get_default_value (ci));
            end case;
            inserted := Ucl.ucl_object_insert_key
              (top      => config_object,
               elt      => obj,
               key      => get_ci_key (ci),
               copy_key => False);
         end;
      end loop;

      if path = "" then
         if DIR.Exists ("/" & default_conf) then
            conffd := Unix.open_file (dirfd         => Context.reveal_root_fd,
                                      relative_path => default_conf,
                                      flags         => open_rdonly);
            if not Unix.file_connected (conffd) then
               EV.emit_with_strerror ("Cannot open /" & default_conf);
            end if;
         end if;
      else
         if DIR.Exists (path) then
            conffd := Unix.open_file (path, open_rdonly);
            if not Unix.file_connected (conffd) then
               EV.emit_with_strerror ("Cannot open " & path);
            end if;
         end if;
      end if;

      parser := Ucl.ucl_parser_new_basic;
      Ucl.ucl_parser_register_variable (parser, get_ci_key (abi), USS (abicalc.abi));

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
               EV.emit_error ("Invalid configuration file: " & Ucl.ucl_parser_get_error (parser));
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
                     EV.emit_error
                       (ukey & " in ravensw.conf is no longer supported.  " &
                          "Convert to the new repository style.  See ravensw.conf(5)");
                     fatal_errors := True;
                  else
                     conitem := Ucl.ucl_object_keyl (config_object, ukey);

                     --  ignore unknown keys
                     if conitem /= null then
                        if not Ucl.object_types_equal (conitem, item) then
                           EV.emit_error ("Malformed key " & key & ", ignoring");
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
               return RESULT_FATAL;
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
                  EV.emit_error ("pkg_ini: unsupported type: " & item.c_type'Img);
            end;
         end loop;

         --  Now overwrite any configuration options defined by command line
         if not IsBlank (options) then
            declare
               numfields : constant Natural := count_char (options, LAT.Vertical_Line) + 1;
            begin
               for F in 1 .. numfields loop
                  declare
                     delimiter : String := LAT.Vertical_Line & "";
                     nvequals  : String := "=";
                     nvpair : String := specific_field (options, F, delimiter);
                  begin
                     if contains (nvpair, nvequals) then
                        declare
                           name : constant String := uppercase (part_1 (nvpair, nvequals));
                           val  : constant String := part_2 (nvpair, nvequals);
                           contype : Config_Entry_Type;
                        begin
                           iter := libucl.ucl_object_iter_t (System.Null_Address);
                           loop
                              item := Ucl.ucl_object_iterate (config_object, iter'Access, True);
                              if item = null then
                                 EV.emit_notice ("option not set (key not found): " & nvpair);
                                 exit;
                              end if;

                              if Ucl.ucl_object_key (item) = name then
                                 contype := convert (item);
                                 obj := convert_string_to_ucl_object (contype, val);

                                 if virgin then
                                    virgin := False;
                                    ncfg := Ucl.ucl_object_typed_new_object;
                                 end if;

                                 inserted :=
                                   Ucl.ucl_object_replace_key
                                     (top      => ncfg,
                                      elt      => obj,
                                      key      => name,
                                      copy_key => True);
                                 exit;
                              end if;
                           end loop;
                        exception
                           when Unsupported_Type =>
                              EV.emit_error ("pkg_ini: unsupported type: " & item.c_type'Img);
                        end;
                     end if;
                  end;
               end loop;
            end;
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
            end loop;
            libucl.ucl_object_unref (ncfg);
         end if;
      end;

      parsed := True;

      if dlevel > 0 then
         --  Let command line option override conf
         context.register_debug_level (dlevel);
      else
         declare
            DL : int64 := config_get_int64 (get_ci_key (debug_level));
         begin
            if DL >= int64 (ST_Debug_Level'First) and then
              DL <= int64 (ST_Debug_Level'Last)
            then
               context.register_debug_level (ST_Debug_Level (DL));
            else
               EV.emit_notice ("DEBUG_LEVEL out of range, ignoring");
            end if;
         end;
      end if;

      --  Above this line, pkg_debug doesn't work.  Use EV.pkg_emit_notice instead, perhaps.
      EV.emit_debug (1, "ravensw initialized");

      --  Start the event pipe
      --  If used, event pipe closed by Core.Finalize.cleanup
      declare
         evpipe : String := config_get_string (get_ci_key (event_pipe));
      begin
         if not IsBlank (evpipe) then
            connect_evpipe (evpipe);
         end if;
      end;

      context.register_dev_mode (config_get_boolean (get_ci_key (dev_mode)));

      declare
         ci_key    : String := get_ci_key (user_agent);
         useragent : String := config_get_string (ci_key);
      begin
         if IsBlank (useragent) then
            ENV.Set (ci_key, "ravensw/" & progversion);
         else
            ENV.Set (ci_key, useragent);
         end if;
      end;

      declare
         obj      : access constant libucl.ucl_object_t;
         iter     : aliased libucl.ucl_object_iter_t :=
                            libucl.ucl_object_iter_t (System.Null_Address);
         item     : access constant libucl.ucl_object_t;
      begin
         obj := config_get (get_ci_key (environ));
         loop
            item := Ucl.ucl_object_iterate (config_object, iter'Access, True);
            exit when item = null;

            declare
               key     : String := Ucl.ucl_object_key (item);
               payload : String := Ucl.ucl_object_tostring_forced (item);
            begin
               EV.emit_debug (1, "Setting env var: " & key);
               if not IsBlank (key) then
                  ENV.Set (key, payload);
               end if;
            end;
         end loop;
      end;

      load_repositories (reposdir, flags);

      --  validate the different scheme
      declare
         procedure scan (position : Repo.Repository_Crate.Cursor);

         invalid_url    : Boolean := False;
         invalid_scheme : Boolean := False;

         procedure scan (position : Repo.Repository_Crate.Cursor)
         is
            repo  : Repo.A_repo renames Repository_Crate.Element (position);
            url   : constant String := pkg_repo_url (repo);
            delim : constant String := ":/";
         begin
            if not invalid_url and then not invalid_scheme then
               if not contains (url, delim) then
                  EV.emit_error ("invalid url: " & url);
                  invalid_url := True;
               end if;

               if not invalid_url then
                  fatal_errors := True;
                  declare
                     sobj   : access constant libucl.ucl_object_t;
                     iter   : aliased libucl.ucl_object_iter_t :=
                                      libucl.ucl_object_iter_t (System.Null_Address);
                     item   : access constant libucl.ucl_object_t;
                     scheme : constant String := part_1 (url, delim);
                  begin
                     sobj := Ucl.ucl_object_find_key (config_object, get_ci_key (valid_scheme));
                     loop
                        exit when not fatal_errors;
                        item := Ucl.ucl_object_iterate (sobj, iter'Access, True);
                        exit when item = null;

                        if Ucl.ucl_object_tostring_forced (item) = scheme then
                           fatal_errors := False;
                        end if;
                     end loop;
                  end;

                  if fatal_errors then
                     EV.emit_error ("invalid scheme " & part_1 (url, delim));
                     invalid_scheme := True;
                  end if;
               end if;
            end if;
         end scan;
      begin
         repositories.Iterate (scan'Access);
         if invalid_url or else invalid_scheme then
            return RESULT_FATAL;
         end if;
      end;

      --  TODO: bypass resolv.conf

      declare
         metalog_filename : String := config_get_string (get_ci_key (metalog_file));
      begin
         if not IsBlank (metalog_filename) then
            if Metalog.metalog_open (metalog_filename) /= RESULT_OK then
               return RESULT_FATAL;
            end if;
         end if;
      end;

      return RESULT_OK;
   end establish_configuration;


   --------------------------------------------------------------------
   --  establish_configuration
   --------------------------------------------------------------------
   function establish_configuration
     (path     : String;
      reposdir : String;
      dlevel   : ST_Debug_Level;
      options  : String) return Action_Result is
   begin
      return establish_configuration (path, reposdir, init_none, dlevel, options);
   end establish_configuration;


end Core.Config.Read;
