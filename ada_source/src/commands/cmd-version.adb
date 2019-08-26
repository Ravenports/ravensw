--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Version;  use Core.Version;
with Core.Strings;  use Core.Strings;
with Core.Unix;

package body Cmd.Version is

   --------------------------------------------------------------------
   --  execute_version_command
   --------------------------------------------------------------------
   function execute_version_command (comline : Cldata) return Boolean is
   begin
      case comline.version_behavior is
         when no_defined_behavior => return False;  --  can't happen
         when use_remote_catalog_state => return False;  --  TBI
         when use_conspiracy_state => return False;
         when test_versions =>
            return do_testversion (USS (comline.version_test1), USS (comline.version_test2));
         when compare_against_pattern =>
            return do_testpattern (USS (comline.version_test1), USS (comline.version_test2));
      end case;
   end execute_version_command;


   --------------------------------------------------------------------
   --  do_testversion
   --------------------------------------------------------------------
   function do_testversion (pkgname1, pkgname2 : String) return Boolean is
   begin
      case pkg_version_cmp (pkgname1, pkgname2) is
         when -1 => TIO.Put_Line ("<");
         when  0 => TIO.Put_Line ("=");
         when  1 => TIO.Put_Line (">");
      end case;
      return True;
   end do_testversion;


   --------------------------------------------------------------------
   --  do_testpattern
   --------------------------------------------------------------------
   function do_testpattern (pkgname, pattern : String) return Boolean is
   begin
      return Unix.filename_match (pattern, pkgname);
   end do_testpattern;

end Cmd.Version;
