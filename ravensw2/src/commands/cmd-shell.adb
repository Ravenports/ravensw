--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

--  with Core.PkgDB;
with Core.Strings; use Core.Strings;

package body Cmd.Shell is

   --------------------------------------------------------------------
   --  execute_shell_command
   --------------------------------------------------------------------
   function execute_shell_command (comline : Cldata) return Boolean is
   begin
      --  TODO PkgDB.pkgdb_command (USS (comline.shell_pass_along));
      return True;
   end execute_shell_command;

end Cmd.Shell;
