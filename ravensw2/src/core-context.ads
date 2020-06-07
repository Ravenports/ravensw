--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Unix;


package Core.Context is

   function reveal_db_directory_fd return Unix.File_Descriptor;
   function reveal_root_fd return Unix.File_Descriptor;
   function reveal_event_pipe return Unix.File_Descriptor;
   function reveal_pkg_rootdir return String;
   function reveal_debug_level return ST_Debug_Level;
   function reveal_developer_mode return Boolean;
   function reveal_jailed return Boolean;
   function reveal_jail_name return String;

   procedure close_eventpipe;
   procedure close_root_fd;
   procedure close_cache_directory_fd;
   procedure close_db_directory_fd;

   procedure register_debug_level (level : ST_Debug_Level);
   procedure register_dev_mode (mode_on : Boolean);
   function register_event_pipe_via_file (pipe_name : String) return Boolean;
   function register_event_pipe_via_socket (pipe_name : String) return Unix.Unix_Socket_Result;

private

   type A_context is
      record
         eventpipe      : Unix.File_Descriptor := Unix.not_connected;
         debug_level    : ST_Debug_Level := ST_Debug_Level'First;
         developer_mode : Boolean        := False;
         jailed         : Boolean        := False;
         pkg_rootdir    : Text;
         cachedir       : Text;
         jail_name      : Text;
         rootfd         : Unix.File_Descriptor := Unix.not_connected;
         cachedirfd     : Unix.File_Descriptor := Unix.not_connected;
         dbdirfd        : Unix.File_Descriptor := Unix.not_connected;
      end record;

   context : A_context;

end Core.Context;
