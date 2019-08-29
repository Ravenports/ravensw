--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Version;  use Core.Version;
with Core.Strings;  use Core.Strings;
with Core.Unix;
with Core.Event;
with Core.PkgDB;

package body Cmd.Version is

   --------------------------------------------------------------------
   --  execute_version_command
   --------------------------------------------------------------------
   function execute_version_command (comline : Cldata) return Boolean
   is
      use_conspiracy : Boolean := False;
      use_catalog    : Boolean := False;
      match          : PkgDB.T_match := PkgDB.MATCH_ALL;
   begin
      case comline.version_behavior is
         when no_defined_behavior =>
            --  happens when no -t, -T, -I, -R, or -r switch set
            declare
               versionsrc : String := pkg_config_get_string (conf_version_source);
               autodetect : Boolean := True;
            begin
               if versionsrc'Length > 0 then
                  case versionsrc (1) is
                     when 'I' =>
                        autodetect := False;
                        use_conspiracy := True;
                     when 'P' =>
                        autodetect := False;
                        use_catalog := True;
                     when others =>
                        TIO.Put_Line
                          (TIO.Standard_Error, "Invalid VERSION_SOURCE in configuration");
                  end case;
               end if;

               if autodetect then
                  null;
               end if;
            end;
         when use_remote_catalog_state =>
            use_catalog := True;
         when use_conspiracy_state =>
            use_conspiracy := True;
         when test_versions =>
            return do_testversion (USS (comline.version_test1), USS (comline.version_test2));
         when compare_against_pattern =>
            return do_testpattern (USS (comline.version_test1), USS (comline.version_test2));
      end case;

      match := PkgDB.set_match_behavior (request_exact     => comline.version_exact_match,
                                         request_glob      => comline.verb_shell_glob,
                                         request_regex     => comline.verb_use_regex);

      if comline.verb_case_sensitive then
         PkgDB.pkgdb_set_case_sensitivity (sensitive => True);
      elsif comline.verb_case_blind then
         PkgDB.pkgdb_set_case_sensitivity (sensitive => False);
      end if;

      if use_conspiracy then
         return do_conspiracy_index
           (match_char  => comline.version_match_char,
            not_char    => comline.version_not_char,
            match       => match,
            pattern     => USS (comline.verb_name_pattern),
            matchorigin => USS (comline.version_origin),
            matchname   => USS (comline.version_pkg_name));
      end if;

      if use_catalog then
         return do_remote_index
           (match_char  => comline.version_match_char,
            not_char    => comline.version_not_char,
            match       => match,
            pattern     => USS (comline.verb_name_pattern),
            matchorigin => USS (comline.version_origin),
            matchname   => USS (comline.version_pkg_name),
            auto_update => not comline.verb_skip_catalog,
            reponame    => USS (comline.verb_repo_name));
      end if;

      return False;  --  Can't happen
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


   --------------------------------------------------------------------
   --  do_conspiracy_index
   --------------------------------------------------------------------
   function do_conspiracy_index
     (match_char  : Character;
      not_char    : Character;
      match       : PkgDB.T_match;
      pattern     : String;
      matchorigin : String;
      matchname   : String) return Boolean
   is
   begin
      --  TODO: Implement
      return False;
   end do_conspiracy_index;


   --------------------------------------------------------------------
   --  do_remote_index
   --------------------------------------------------------------------
   function do_remote_index
     (match_char  : Character;
      not_char    : Character;
      match       : PkgDB.T_match;
      pattern     : String;
      matchorigin : String;
      matchname   : String;
      auto_update : Boolean;
      reponame    : String) return Boolean
   is
   begin
      --  TODO: Implement
      return False;
   end do_remote_index;

end Cmd.Version;
