--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings; use Core.Strings;
with Core.Database.Operations;

package body Cmd.Shell is

   --------------------------------------------------------------------
   --  execute_shell_command
   --------------------------------------------------------------------
   function execute_shell_command (comline : Cldata) return Boolean is
   begin
      Database.Operations.start_shell (USS (comline.shell_pass_along));
      return True;
   end execute_shell_command;

end Cmd.Shell;
