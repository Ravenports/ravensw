--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Environment_Variables;
with Core.Event;
with Core.Unix;
with Core.Strings;
with Core.Object;
with Core.Metalog;
with System;

package body Core.Config is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package ENV renames Ada.Environment_Variables;
   package EV  renames Core.Event;
   package CS  renames Core.Strings;

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
      reposdir : String) return Pkg_Error_Type is
   begin
      return pkg_ini (path, reposdir, init_none);
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
      return Object.pkg_object_string (pkg_config_get (key));
   end pkg_config_get_string;


   --------------------------------------------------------------------
   --  pkg_config_get_boolean
   --------------------------------------------------------------------
   function pkg_config_get_boolean (key : String) return Boolean is
   begin
      return Object.pkg_object_bool (pkg_config_get (key));
   end pkg_config_get_boolean;


   --------------------------------------------------------------------
   --  pkg_config_get_int64
   --------------------------------------------------------------------
   function pkg_config_get_int64 (key : String) return Ucl.int64 is
   begin
      return Object.pkg_object_int (pkg_config_get (key));
   end pkg_config_get_int64;


   --------------------------------------------------------------------
   --  pkg_ini
   --------------------------------------------------------------------
   function pkg_ini
     (path     : String;
      reposdir : String;
      flags    : Pkg_init_flags) return Pkg_Error_Type
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
               libucl.ucl_object_unref (ncfg);
            end loop;
         end if;
      end;

      parsed := True;

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

      --  TODO: load the repositories
      --  TODO: validate the different scheme
      --  TODO: bypass resolv.conf

      declare
         metalog_file : String :=  Object.pkg_object_string (pkg_config_get ("METALOG"));
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
                  for k in 1 .. nf loop
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
                  for k in 1 .. nf loop
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

end Core.Config;
