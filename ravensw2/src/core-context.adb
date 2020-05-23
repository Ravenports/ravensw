--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Config;  use Core.Config;
with Core.Strings; use Core.Strings;

package body Core.Context is


   --------------------------------------------------------------------
   --  reveal_db_directory_fd
   --------------------------------------------------------------------
   function reveal_db_directory_fd return Unix.File_Descriptor is
   begin
      if not Unix.file_connected (context.dbdirfd) then
         declare
            db_dir : String := configuration_value (dbdir);
            flags : Unix.T_Open_Flags := (DIRECTORY => True,
                                          CLOEXEC => True,
                                          others => False);
         begin
            context.dbdirfd := Unix.open_file (db_dir, flags);
         end;
      end if;
      return context.dbdirfd;
   end reveal_db_directory_fd;


   --------------------------------------------------------------------
   --  reveal_root_fd
   --------------------------------------------------------------------
   function reveal_root_fd return Unix.File_Descriptor is
   begin
      if not Unix.file_connected (context.rootfd) then
         declare
            flags : Unix.T_Open_Flags;
         begin
            flags.DIRECTORY := True;
            flags.RDONLY    := True;
            flags.CLOEXEC   := True;
            context.rootfd := Unix.open_file ("/", flags);
            --  Caller has to check for success.
         end;
      end if;
      return context.rootfd;
   end reveal_root_fd;


   --------------------------------------------------------------------
   --  reveal_event_pipe
   --------------------------------------------------------------------
   function reveal_event_pipe return Unix.File_Descriptor is
   begin
      return context.eventpipe;
   end reveal_event_pipe;


   --------------------------------------------------------------------
   --  reveal_developer_mode
   --------------------------------------------------------------------
   function reveal_developer_mode return Boolean is
   begin
      return context.developer_mode;
   end reveal_developer_mode;


   --------------------------------------------------------------------
   --  register_event_pipe_via_file
   --------------------------------------------------------------------
   function register_event_pipe_via_file (pipe_name : String) return Boolean
   is
      sock_flags  : Unix.T_Open_Flags;
   begin
      if Unix.file_connected (context.eventpipe) then
         return True;
      else
         sock_flags.WRONLY := True;
         sock_flags.NON_BLOCK := True;
         context.eventpipe := Unix.open_file (pipe_name, sock_flags);
         return Unix.file_connected (context.eventpipe);
      end if;
   end register_event_pipe_via_file;


   --------------------------------------------------------------------
   --  register_event_pipe_via_socket
   --------------------------------------------------------------------
   function register_event_pipe_via_socket (pipe_name : String) return Unix.Unix_Socket_Result
   is
   begin
      if Unix.file_connected (context.eventpipe) then
         return Unix.connected;
      end if;

      --  Caller has to handle failure
      return Unix.connect_unix_socket (pipe_name, context.eventpipe);
   end register_event_pipe_via_socket;


   --------------------------------------------------------------------
   --  close_eventpipe
   --------------------------------------------------------------------
   procedure close_eventpipe
   is
      success : Boolean;
   begin
      success := Unix.close_file (context.eventpipe);
   end close_eventpipe;


   --------------------------------------------------------------------
   --  close_root_fd
   --------------------------------------------------------------------
   procedure close_root_fd
   is
      success : Boolean;
   begin
      success := Unix.close_file (context.rootfd);
   end close_root_fd;


   --------------------------------------------------------------------
   --  close_cache_directory_fd
   --------------------------------------------------------------------
   procedure close_cache_directory_fd
   is
      success : Boolean;
   begin
      success := Unix.close_file (Context.cachedirfd);
   end close_cache_directory_fd;


   --------------------------------------------------------------------
   --  close_db_directory_fd
   --------------------------------------------------------------------
   procedure close_db_directory_fd
   is
      success : Boolean;
   begin
      success := Unix.close_file (context.dbdirfd);
   end close_db_directory_fd;


   --------------------------------------------------------------------
   --  reveal_pkg_rootdir
   --------------------------------------------------------------------
   function reveal_pkg_rootdir return String is
   begin
      return USS (context.pkg_rootdir);
   end reveal_pkg_rootdir;


   --------------------------------------------------------------------
   --  reveal_debug_level
   --------------------------------------------------------------------
   function reveal_debug_level return ST_Debug_Level is
   begin
      return Context.debug_level;
   end reveal_debug_level;


   --------------------------------------------------------------------
   --  register_debug_level
   --------------------------------------------------------------------
   procedure register_debug_level (level : ST_Debug_Level) is
   begin
      Context.debug_level := level;
   end register_debug_level;


   --------------------------------------------------------------------
   --  register_dev_mode
   --------------------------------------------------------------------
   procedure register_dev_mode (mode_on : Boolean) is
   begin
      Context.developer_mode := mode_on;
   end register_dev_mode;


end Core.Context;
