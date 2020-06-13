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

      option_verbose  := comline.verb_verbose;
      option_origin   := comline.version_disp_origin;
      option_name     := not IsBlank (comline.version_pkg_name);
      option_status   := (comline.version_not_char /= Character'First);
      option_nostatus := (comline.version_match_char /= Character'First);


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
   procedure print_version
     (pkg_version  : String;
      pkg_name     : String;
      pkg_origin   : String;
      source       : String;
      ver          : String;
      limchar      : Character)
   is
      key : Character;
   begin
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

      if option_status and then limchar /= key then
         return;
      end if;

      if option_nostatus and then limchar = key then
         return;
      end if;

      if option_origin then
         TIO.Put (pad_right (pkg_origin, 34) & " " & key);
      else
         TIO.Put (pad_right (pkg_name & "-" & pkg_version, 34) & " " & key);
      end if;

      if option_verbose then
         case key is
            when '<' => TIO.Put ("   needs updating (" & source & " has " & ver & ")");
            when '=' => TIO.Put ("   up-to-date with " & source);
            when '>' => TIO.Put ("   succeeds " & source & " (" & source & " has " & ver & ")");
            when '?' => TIO.Put ("   orphaned: " & pkg_origin);
            when '!' => TIO.Put ("   Comparison failed");
            when others => TIO.Put_Line ("?????");
         end case;
      end if;
      TIO.Put_Line ("");
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
      function compare_remote (this_repo : String;
                               local_pkg : Pkgtypes.A_Package;
                               is_origin : in out Boolean)
                               return Boolean;

      retcode : Action_Result;
      db      : Database.RDB_Connection;
      all_ok  : Boolean := True;

      procedure release_db is
      begin
         if not DBO.rdb_release_lock (db, Database.RDB_LOCK_READONLY) then
            Event.emit_error ("Cannot release read lock on database.");
         end if;
         DBO.rdb_close (db);
      end release_db;

      function compare_remote (this_repo : String;
                               local_pkg : Pkgtypes.A_Package;
                               is_origin : in out Boolean) return Boolean
      is
         loc_name    : String := Printf.format_attribute (local_pkg, Printf.PKG_NAME);
         loc_origin  : String := Printf.format_attribute (local_pkg, Printf.PKG_ORIGIN);
         loc_version : String := Printf.format_attribute (local_pkg, Printf.PKG_VERSION);
         skip        : Boolean := False;
         it_remote   : Repo.Iterator.Packages.SQLite_Iterator;
         remote_pkg  : aliased Pkgtypes.A_Package;
         rem_pattern : Text;
      begin
         if option_origin then
            if loc_origin /= matchorigin then
               skip := True;
               is_origin := True;
            end if;
         elsif option_name then
            if loc_name /= matchname then
               skip := True;
               is_origin := False;
            end if;
         end if;
         if not skip then
            if is_origin then
               rem_pattern := SUS (loc_origin);
            else
               rem_pattern := SUS (loc_name);
            end if;
            if it_remote.initialize_as_standard_query (reponame => this_repo,
                                                       pattern  => USS (rem_pattern),
                                                       match    => Database.MATCH_EXACT,
                                                       just_one => True) = RESULT_OK
            then
               case it_remote.Next (pkg_access => remote_pkg'Unchecked_Access,
                                    sections   => (Pkgtypes.basic => True, others => False))
               is
                  when RESULT_OK =>
                     print_version
                       (pkg_version => loc_version,
                        pkg_name    => loc_name,
                        pkg_origin  => loc_origin,
                        source      => "remote",
                        ver         => Printf.format_attribute (remote_pkg, Printf.PKG_VERSION),
                        limchar     => match_char);

                  when others =>
                     print_version
                       (pkg_version => loc_version,
                        pkg_name    => loc_name,
                        pkg_origin  => loc_origin,
                        source      => "remote",
                        ver         => "",
                        limchar     => not_char);
               end case;
            else
               return False;
            end if;
         end if;
         return True;
      end compare_remote;

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
         return False;
      end if;

      if not DBO.rdb_obtain_lock (db, Database.RDB_LOCK_READONLY) then
         Event.emit_error ("Cannot get a read lock on database. It is locked by another process.");
         DBO.rdb_close (db);
         return False;
      end if;

      declare
         procedure scan (Position : Repo.Active_Repository_Name_Set.Cursor);

         active : Repo.Active_Repository_Name_Set.Vector;

         check_origin : Boolean := not IsBlank (matchorigin);
         check_name   : Boolean := not IsBlank (matchname);
         is_origin    : Boolean := False;

         procedure scan (Position : Repo.Active_Repository_Name_Set.Cursor)
         is
            rname  : Text renames Repo.Active_Repository_Name_Set.Element (Position);
            it     : Database.Iterator.DB_SQLite_Iterator;
            my_pkg : aliased Pkgtypes.A_Package;
         begin
            if all_ok then
               if it.initialize_as_standard_query (conn     => db,
                                                   pattern  => pattern,
                                                   match    => match,
                                                   just_one => False) /= RESULT_OK
               then
                  Event.emit_error ("Failed to initialize SQLite basic iterator (local)");
                  all_ok := False;
               end if;
            end if;
            if all_ok then
               loop
                  --  Next can be OK/END/FATAL
                  case it.Next (pkg_access => my_pkg'Unchecked_Access,
                                sections   => (Pkgtypes.basic => True, others => False))
                  is
                     when RESULT_END =>
                        exit;

                     when RESULT_OK =>
                        if not compare_remote (USS (rname), my_pkg, is_origin) then
                           Event.emit_error ("Failed to initialize remote pkg iterator");
                           all_ok := False;
                        end if;

                     when others =>
                        Event.emit_error ("Failed to retrieve package in version cmd");
                        all_ok := False;
                  end case;
               end loop;
            end if;
         end scan;

      begin
         if IsBlank (reponame) then
            active := Repo.ordered_active_repositories;
         else
            active.Append (SUS (reponame));
         end if;
         active.Iterate (scan'Access);
         release_db;
         return all_ok;
      end;
   end do_remote_index;

end Cmd.Version;
