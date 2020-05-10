--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;  use Core.Strings;
with Core.Unix;
with Core.Config;
with Core.Event;
with Core.Version;
--  with Core.PkgDB_Query;
--  with Core.Printf;
--  with Cmd.Update;

package body Cmd.Version is

   package EV  renames Core.Event;
   package VER renames Core.Version;

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
                        EV.emit_notice ("Invalid VERSION_SOURCE in configuration: " & versionsrc);
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

--        if use_conspiracy then
--           return do_conspiracy_index
--             (match_char  => comline.version_match_char,
--              not_char    => comline.version_not_char,
--              match       => match,
--              pattern     => USS (comline.verb_name_pattern),
--              matchorigin => USS (comline.version_origin),
--              matchname   => USS (comline.version_pkg_name));
--        end if;
--
--        if use_catalog then
--           return do_remote_index
--             (match_char  => comline.version_match_char,
--              not_char    => comline.version_not_char,
--              match       => match,
--              pattern     => USS (comline.verb_name_pattern),
--              matchorigin => USS (comline.version_origin),
--              matchname   => USS (comline.version_pkg_name),
--              auto_update => not comline.verb_skip_catalog,
--              quiet       => comline.verb_quiet,
--              reponame    => USS (comline.verb_repo_name));
--        end if;

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
--     --------------------------------------------------------------------
--     --  do_remote_index
--     --------------------------------------------------------------------
--     function do_remote_index
--       (match_char  : Character;
--        not_char    : Character;
--        match       : PkgDB.T_match;
--        pattern     : String;
--        matchorigin : String;
--        matchname   : String;
--        auto_update : Boolean;
--        quiet       : Boolean;
--        reponame    : String) return Boolean
--     is
--        procedure cleanup;
--
--        retcode : Pkg_Error_Type;
--        db      : PkgDB.struct_pkgdb;
--
--        procedure cleanup is
--        begin
--           if PkgDB.pkgdb_release_lock (db, PkgDB.PKGDB_LOCK_READONLY) then
--              null;
--           end if;
--           PkgDB.pkgdb_close (db);
--        end cleanup;
--     begin
--        if auto_update then
--           retcode := Cmd.Update.pkgcli_update (force    => False,
--                                                strict   => False,
--                                                quiet    => quiet,
--                                                reponame => reponame);
--           if retcode /= EPKG_OK then
--              return False;
--           end if;
--        end if;
--
--        if PkgDB.pkgdb_open_all (db       => db,
--                                 dbtype   => PkgDB.PKGDB_REMOTE,
--                                 reponame => reponame) /= EPKG_OK
--        then
--           return False;
--        end if;
--
--        if not PkgDB.pkgdb_obtain_lock (db, PkgDB.PKGDB_LOCK_READONLY) then
--           PkgDB.pkgdb_close (db);
--           TIO.Put_Line
--             (TIO.Standard_Error,
--              "Cannot get a read lock on a database. It is locked by another process");
--           return False;
--        end if;
--
--        declare
--           iter : IBS.Iterator_Binary_Sqlite;
--           pkg  : T_pkg_Access;
--           check_origin : Boolean := not IsBlank (matchorigin);
--           check_name   : Boolean := not IsBlank (matchname);
--           good_result  : Boolean := True;
--        begin
--           iter := PkgDB_Query.pkgdb_query (db, pattern, match);
--           if iter.invalid_iterator then
--              cleanup;
--              return False;
--           end if;
--
--           loop
--              exit when iter.Next (pkg, Iterators.PKG_LOAD_FLAG_BASIC) /= EPKG_OK;
--              declare
--                 name   : constant String := Printf.format_attribute (pkg.all, Printf.PKG_NAME);
--                 origin : constant String := Printf.format_attribute (pkg.all, Printf.PKG_ORIGIN);
--                 skip   : Boolean := False;
--
--                 iter_remote : IBS.Iterator_Binary_Sqlite;
--              begin
--                 if check_origin then
--                    if origin /= matchorigin then
--                       skip := True;
--                    end if;
--                 elsif check_name then
--                    if name /= matchname then
--                       skip := True;
--                    end if;
--                 end if;
--
--                 if not skip then
--                    --  TODO: it_remote = pkgdb_repo_query
--                    --     (db, is_origin ? origin : name, MATCH_EXACT, reponame);
--                    if iter_remote.invalid_iterator then
--                       good_result := False;
--                       exit;
--                    end if;
--
--                    --  loop
--                    --  end loop
--                    IBS.Free (iter_remote);
--                 end if;
--              end;
--              delete_pkg (pkg);
--           end loop;
--
--           IBS.Free (iter);
--           cleanup;
--           return good_result;
--        end;
--
--     end do_remote_index;

end Cmd.Version;
