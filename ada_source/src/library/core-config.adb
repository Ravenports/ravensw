--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Core.Event;
with Core.Unix;

package body Core.Config is

   package DIR renames Ada.Directories;
   package EV  renames Core.Event;

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
   function pkg_config_get (key : String) return Ucl.pkg_object is
   begin
      return Ucl.ucl_object_find_key (config_object, key);
   end pkg_config_get;


   --------------------------------------------------------------------
   --  pkg_ini
   --------------------------------------------------------------------
   function pkg_ini
     (path     : String;
      reposdir : String;
      flags    : Pkg_init_flags) return Pkg_Error_Type is
   begin
      if parsed then
         return EPKG_FATAL;
      end if;

      --  ...
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

      use type Unix.Unix_File_Descriptor;
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
            context.eventpipe := Unix.open (filename  => event_pipe,
                                            WRONLY    => True,
                                            NON_BLOCK => True);
            if context.eventpipe = Unix.not_connected then
               EV.pkg_emit_errno (SUS ("open event pipe (FIFO)"), SUS (event_pipe), Unix.errno);
            end if;

         when Unix.unix_socket =>
            declare
               fd : Unix.Unix_File_Descriptor;
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

end Core.Config;
