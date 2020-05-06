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



end Core.Context;
