--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;  use Core.Strings;
with Core.Unix;
with Core.Config;
with Core.Event;
with Core.Version;
with Core.Database.Operations;
with Core.Database.Iterator;
with Core.Repo.Iterator.Packages;
with Core.Printf;
with Core.Pkgtypes;
with Cmd.Update;

package body Cmd.Version is

   package VER renames Core.Version;
   package DBO renames Core.Database.Operations;
   package RIT renames Core.Repo.Iterator.Packages;

   --------------------------------------------------------------------
   --  execute_version_command
   --------------------------------------------------------------------
   function execute_version_command (comline : Cldata) return Boolean
   is
      use_conspiracy : Boolean := False;
      use_catalog    : Boolean := False;
      match          : Database.Match_Behavior := Database.MATCH_ALL;
   begin
      case comline.version_behavior is
         when no_defined_behavior =>
            --  happens when no -t, -T, -I, -R, or -r switch set
            declare
               versionsrc : String := Config.configuration_value (config.version_source);
               autodetect : Boolean := True;
            begin
               if versionsrc'Length > 0 then
                  case versionsrc (versionsrc'First) is
                     when 'I' =>
                        autodetect := False;
                        use_conspiracy := True;
                     when 'P' =>
                        autodetect := False;
                        use_catalog := True;
                     when others =>
                        Event.emit_notice
                          ("Invalid VERSION_SOURCE in configuration: " & versionsrc);
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

      match := Database.set_match_behavior (request_exact     => comline.version_exact_match,
                                            request_glob      => comline.verb_shell_glob,
                                            request_regex     => comline.verb_use_regex);

      if comline.verb_case_sensitive then
         Database.set_case_sensitivity (sensitive => True);
      elsif comline.verb_case_blind then
         Database.set_case_sensitivity (sensitive => False);
      end if;

      option_verbose := comline.verb_verbose;
      option_origin  := comline.version_disp_origin;
      option_name    := not IsBlank (comline.version_pkg_name);

      --  -l/-L are mutually exclusive and both can't be set.
      option_match_status := (comline.version_match_char /= Character'First);
      if option_match_status then
         option_cmp_operator := comline.version_match_char;
         option_avoid_status := False;
      else
         option_avoid_status := (comline.version_not_char /= Character'First);
         if option_avoid_status then
            option_cmp_operator := comline.version_not_char;
         end if;
      end if;

--        if use_conspiracy then
--           return do_conspiracy_index
--             (match_char  => comline.version_match_char,
--              not_char    => comline.version_not_char,
--              match       => match,
--              pattern     => USS (comline.verb_name_pattern),
--              matchorigin => USS (comline.version_origin),
--              matchname   => USS (comline.version_pkg_name));
--        end if;

      if use_catalog then
         return do_remote_index
           (match_char  => comline.version_match_char,
            not_char    => comline.version_not_char,
            match       => match,
            pattern     => USS (comline.verb_name_pattern),
            matchorigin => USS (comline.version_origin),
            matchname   => USS (comline.version_pkg_name),
            auto_update => not comline.verb_skip_catalog,
            quiet       => comline.verb_quiet,
            reponame    => USS (comline.verb_repo_name));
      end if;

      return False;  --  Can't happen
   end execute_version_command;


   --------------------------------------------------------------------
   --  do_testversion
   --------------------------------------------------------------------
   function do_testversion (pkgname1, pkgname2 : String) return Boolean is
   begin
      case VER.pkg_version_cmp (pkgname1, pkgname2) is
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
   --  print_version
   --------------------------------------------------------------------
   function print_version
     (pkg_version  : String;
      pkg_name     : String;
      pkg_origin   : String;
      source       : String;
      ver          : String) return Display_Line
   is
      key  : Character;
      line : Display_Line;
   begin
      line.valid := False;
      if IsBlank (ver) then
         if IsBlank (source) then
            key := '!';
         else
            key := '?';
         end if;
      else
         case Core.Version.pkg_version_cmp (pkg_version, ver) is
            when -1 => key := '<';
            when  0 => key := '=';
            when  1 => key := '>';
         end case;
      end if;

      if option_match_status and then option_cmp_operator /= key then
         --  skip records where the key does not match given character
         return line;
      end if;

      if option_avoid_status and then option_cmp_operator = key then
         --  skip records where the key matches given character
         return line;
      end if;

      line.comparison := key;
      if option_origin then
         line.identifier := SUS (pkg_origin);
      else
         line.identifier := SUS (pkg_name & "-" & pkg_version);
      end if;

      if option_verbose then
         case key is
            when '<' =>
               line.extra_info := SUS ("needs updating (" & source & " has " & ver & ")");
            when '=' =>
               line.extra_info := SUS ("up-to-date with " & source);
            when '>' =>
               line.extra_info := SUS ("newer (" & source & " has " & ver & ")");
            when '?' =>
               line.extra_info := SUS ("orphaned: " & pkg_origin);
            when '!' =>
               line.extra_info := SUS ("Comparison failed");
            when others =>
               line.extra_info := SUS ("?????");
         end case;
      end if;
      line.valid := True;
      return line;
   end print_version;


--     --------------------------------------------------------------------
--     --  do_conspiracy_index
--     --------------------------------------------------------------------
--     function do_conspiracy_index
--       (match_char  : Character;
--        not_char    : Character;
--        match       : PkgDB.T_match;
--        pattern     : String;
--        matchorigin : String;
--        matchname   : String) return Boolean
--     is
--     begin
--        --  TODO: Implement
--        return False;
--     end do_conspiracy_index;
--
--
   --------------------------------------------------------------------
   --  do_remote_index
   --------------------------------------------------------------------
   function do_remote_index
     (match_char  : Character;
      not_char    : Character;
      match       : Database.Match_Behavior;
      pattern     : String;
      matchorigin : String;
      matchname   : String;
      auto_update : Boolean;
      quiet       : Boolean;
      reponame    : String) return Boolean
   is
      procedure release_db;
      procedure print (Position : Line_Crate.Cursor);
      procedure iterator_initialize (Position : Repo.Active_Repository_Name_Set.Cursor);
      function compare_remote (this_repo : String;
                               local_pkg : Pkgtypes.A_Package) return Boolean;

      retcode : Action_Result;
      db      : Database.RDB_Connection;
      all_ok  : Boolean := True;
      lines   : Line_Crate.Vector;
      cyclers : array (1 .. Repo.count_of_active_repositories) of RIT.SQLite_Iterator;
      leftlen : Natural := 0;
      cyindex : Natural;

      procedure release_db is
      begin
         if not DBO.rdb_release_lock (db, Database.RDB_LOCK_READONLY) then
            Event.emit_error ("Cannot release read lock on database.");
         end if;
         DBO.rdb_close (db);
      end release_db;

      function compare_remote (this_repo : String;
                               local_pkg : Pkgtypes.A_Package) return Boolean
      is
         loc_name    : String := Printf.format_attribute (local_pkg, Printf.PKG_NAME);
         loc_origin  : String := Printf.format_attribute (local_pkg, Printf.PKG_ORIGIN);
         loc_version : String := Printf.format_attribute (local_pkg, Printf.PKG_VERSION);
         skip        : Boolean := False;
         remote_pkg  : aliased Pkgtypes.A_Package;
         new_line    : Display_Line;
      begin
         cyindex := cyindex + 1;
         if option_origin then
            cyclers (cyindex).rebind (loc_origin);
            if loc_origin /= matchorigin then
               skip := True;
            end if;
         else
            cyclers (cyindex).rebind (loc_name);
            if option_name then
               if loc_name /= matchname then
                  skip := True;
               end if;
            end if;
         end if;
         if not skip then
            case cyclers (cyindex).Next
              (pkg_access => remote_pkg'Unchecked_Access,
               sections   => (Pkgtypes.basic => True, others => False))
            is
               when RESULT_OK =>
                  new_line :=
                    print_version
                      (pkg_version => loc_version,
                       pkg_name    => loc_name,
                       pkg_origin  => loc_origin,
                       source      => "remote",
                       ver         => Printf.format_attribute (remote_pkg, Printf.PKG_VERSION));

               when others =>
                  new_line :=
                    print_version
                      (pkg_version => loc_version,
                       pkg_name    => loc_name,
                       pkg_origin  => loc_origin,
                       source      => "remote",
                       ver         => "");
            end case;
            if new_line.valid then
               if leftlen < SU.Length (new_line.identifier) then
                  leftlen := SU.Length (new_line.identifier);
               end if;
               lines.Append (new_line);
            end if;
            return True;
         else
            return False;
         end if;
      end compare_remote;

      procedure print (Position : Line_Crate.Cursor)
      is
         item : Display_Line renames Line_Crate.Element (Position);
      begin
         TIO.Put_Line (pad_right (USS (item.identifier), leftlen)
                       & " " & item.comparison
                       & "  " & USS (item.extra_info));

      end print;

      procedure iterator_initialize (Position : Repo.Active_Repository_Name_Set.Cursor)
      is
         rname : Text renames Repo.Active_Repository_Name_Set.Element (Position);
      begin
         cyindex := cyindex + 1;
         --  pattern "XXX" indicates it's not an origin or "~" pattern
         if cyclers (cyindex).initialize_as_standard_query
           (reponame => USS (rname),
            pattern  => "XXX",
            match    => Database.MATCH_EXACT,
            just_one => True) /= RESULT_OK
         then
            Event.emit_error ("Failed to initialize " & USS (rname) & " repository iterator");
            all_ok := False;
         end if;
      end iterator_initialize;


   begin
      if auto_update then
         --  reponame might be blank.  This means use ALL databases
         retcode := Cmd.Update.pkgcli_update (force    => False,
                                              strict   => False,
                                              quiet    => quiet,
                                              reponame => reponame);
         if retcode /= RESULT_OK then
            return False;
         end if;
      end if;

      if DBO.rdb_open_all (db, Database.RDB_REMOTE) /= RESULT_OK then
         Event.emit_notice ("At least one database failed to open, exiting.");
         return False;
      end if;

      if not DBO.rdb_obtain_lock (db, Database.RDB_LOCK_READONLY) then
         Event.emit_error ("Cannot get a read lock on database. It is locked by another process.");
         DBO.rdb_close (db);
         return False;
      end if;

      declare
         active : Repo.Active_Repository_Name_Set.Vector;
         it     : Database.Iterator.DB_SQLite_Iterator;
      begin
         if IsBlank (reponame) then
            active := Repo.ordered_active_repositories;
         else
            active.Append (SUS (reponame));
         end if;
         --  set up Repo iterators (one for each active repository)
         cyindex := 0;
         active.Iterate (iterator_initialize'Access);
         if not all_ok then
            return False;
         end if;

         if it.initialize_as_standard_query (conn     => db,
                                             pattern  => pattern,
                                             match    => match,
                                             just_one => False) /= RESULT_OK
         then
            Event.emit_error ("Failed to initialize SQLite basic iterator (local)");
            return False;
         end if;

         loop
            declare
               procedure scan (Position : Repo.Active_Repository_Name_Set.Cursor);

               my_pkg : aliased Pkgtypes.A_Package;

               procedure scan (Position : Repo.Active_Repository_Name_Set.Cursor)
               is
                  rname  : Text renames Repo.Active_Repository_Name_Set.Element (Position);
               begin
                  if all_ok then
                     if not compare_remote (USS (rname), my_pkg) then
                        Event.emit_error ("Failed to initialize remote pkg iterator");
                        all_ok := False;
                     end if;
                  end if;
               end scan;
            begin
               --  Next can be OK/END/FATAL
               case it.Next (pkg_access => my_pkg'Unchecked_Access,
                             sections   => (Pkgtypes.basic => True, others => False))
               is
                  when RESULT_END =>
                     exit;

                  when RESULT_OK =>
                     cyindex := 0;
                     active.Iterate (scan'Access);

                  when others =>
                     Event.emit_error ("Failed to retrieve package in version cmd");
                     all_ok := False;

               end case;
            end;
            exit when not all_ok;
         end loop;
         release_db;

         lines.Iterate (print'Access);
         return all_ok;
      end;
   end do_remote_index;

end Cmd.Version;
