--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Config;  use Core.Config;

package body Core.Context is


   --------------------------------------------------------------------
   --  reveal_db_directory_fd
   --------------------------------------------------------------------
   function reveal_db_directory_fd return Unix.File_Descriptor is
   begin
      if not Unix.file_connected (context.dbdirfd) then
         declare
            dbdir : String := config_get_string (conf_dbdir);
            flags : Unix.T_Open_Flags := (DIRECTORY => True,
                                          CLOEXEC => True,
                                          others => False);
         begin
            context.dbdirfd := Unix.open_file (dbdir, flags);
         end;
      end if;
      return context.dbdirfd;
   end reveal_db_directory_fd;


end Core.Context;
