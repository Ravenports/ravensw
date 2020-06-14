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

   option_verbose : Boolean;
   option_origin  : Boolean;
   option_name    : Boolean;
   option_match_status : Boolean;
   option_avoid_status : Boolean;
   option_cmp_operator : Character;

   type Display_Line is
      record
         identifier : Text;
         comparison : Character;
         extra_info : Text;
         valid      : Boolean;
      end record;

   package Line_Crate is new CON.Vectors
     (Element_Type => Display_Line,
      Index_Type   => Natural);

   --  output
   function print_version
     (pkg_version  : String;
      pkg_name     : String;
      pkg_origin   : String;
      source       : String;
      ver          : String) return Display_Line;

   --  ravensw -R [-l limchar | -L limchar] [-O origin | -n pkgname] [-r reponame]
   --  [-U] [-e|-g|-x] pattern
   function do_remote_index
     (match_char  : Character;
      not_char    : Character;
      match       : Database.Match_Behavior;
      pattern     : String;
      matchorigin : String;
      matchname   : String;
      auto_update : Boolean;
      quiet       : Boolean;
      reponame    : String) return Boolean;


   --  ravensw -I [-l limchar | -L limchar] [-O origin | -n pkgname] [-e|-g|-x] pattern
--     function do_conspiracy_index
--       (match_char  : Character;
--        not_char    : Character;
--        match       : PkgDB.T_match;
--        pattern     : String;
--        matchorigin : String;
--        matchname   : String) return Boolean;
--

end Cmd.Version;
