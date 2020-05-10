--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Interfaces.C.Strings;

package Core.Database.Operations is

   procedure start_shell (arguments : String);

private

   package ICS renames Interfaces.C.Strings;

   procedure pkgshell_open (reponame : access ICS.chars_ptr);
   pragma Export (C, pkgshell_open);


end Core.Database.Operations;
