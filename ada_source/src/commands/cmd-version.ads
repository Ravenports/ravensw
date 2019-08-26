--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Cmd.Version is

   function execute_version_command (comline : Cldata) return Boolean;

private

   --  ravensw version -t <pkg1> <pkg2>
   function do_testversion (pkgname1, pkgname2 : String) return Boolean;

end Cmd.Version;
