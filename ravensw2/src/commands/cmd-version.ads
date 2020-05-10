--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Database;

package Cmd.Version is

   function execute_version_command (comline : Cldata) return Boolean;

private

   --  ravensw version -t <pkg1> <pkg2>
   function do_testversion (pkgname1, pkgname2 : String) return Boolean;

   --  ravensw version -T <pkgname> <pattern>
   function do_testpattern (pkgname, pattern : String) return Boolean;

   --  ravensw -I [-l limchar | -L limchar] [-O origin | -n pkgname] [-e|-g|-x] pattern
--     function do_conspiracy_index
--       (match_char  : Character;
--        not_char    : Character;
--        match       : PkgDB.T_match;
--        pattern     : String;
--        matchorigin : String;
--        matchname   : String) return Boolean;
--
--     --  ravensw -R [-l limchar | -L limchar] [-O origin | -n pkgname] [-r reponame]
--     --  [-U] [-e|-g|-x] pattern
--     function do_remote_index
--       (match_char  : Character;
--        not_char    : Character;
--        match       : PkgDB.T_match;
--        pattern     : String;
--        matchorigin : String;
--        matchname   : String;
--        auto_update : Boolean;
--        quiet       : Boolean;
--        reponame    : String) return Boolean;
end Cmd.Version;
