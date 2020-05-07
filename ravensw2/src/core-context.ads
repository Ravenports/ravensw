--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Unix;


package Core.Context is

   function reveal_db_directory_fd return Unix.File_Descriptor;
   function reveal_pkg_rootdir return String;
   function reveal_debug_level return ST_Debug_Level;

   procedure close_eventpipe;
   procedure close_root_fd;
   procedure close_cache_directory_fd;
   procedure close_db_directory_fd;

   procedure register_debug_level (level : ST_Debug_Level);

private

   type A_context is
      record
         eventpipe      : Unix.File_Descriptor := Unix.not_connected;
         debug_level    : ST_Debug_Level := ST_Debug_Level'First;
         developer_mode : Boolean        := False;
         pkg_rootdir    : Text;
         cachedir       : Text;
         rootfd         : Unix.File_Descriptor := Unix.not_connected;
         cachedirfd     : Unix.File_Descriptor := Unix.not_connected;
         dbdirfd        : Unix.File_Descriptor := Unix.not_connected;
      end record;

   context : A_context;

end Core.Context;
