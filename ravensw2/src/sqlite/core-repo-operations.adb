--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Directories;

with Core.Event;
with Core.Context;
with Core.Unix;
with Core.Repo.Meta;
with SQLite;

package body Core.Repo.Operations is

   package DIR renames Ada.Directories;

   --------------------------------------------------------------------
      --  close_repository
   --------------------------------------------------------------------
   procedure close_repository (reponame : Text; commit : Boolean)
   is
      procedure close_database (key : Text; Element : in out A_repo);
      procedure close_database (key : Text; Element : in out A_repo)
      is
         repository : A_repo renames Element;
      begin
         SQLite.close_database (repository.sqlite_handle);
         repository.sqlite_handle := null;
      end close_database;
   begin
      if repositories.Contains (reponame) then

         if not SQLite.db_connected (repositories.Element (reponame).sqlite_handle) then
            return;
         end if;

         if commit then
            if not trax_commit (repositories.Element (reponame).sqlite_handle, "") then
               Event.emit_error ("close_repository(): Failed to commit transaction");
               return;
            end if;
         end if;

         for S in repository_stmt_index loop
            SQLite.finalize_statement (prepared_statements (S));
         end loop;

         repositories.Update_Element (Position => repositories.Find (reponame),
                                      Process  => close_database'Access);
      else
         raise invalid_repo_name;
      end if;
   end close_repository;


   --------------------------------------------------------------------
   --  ERROR_SQLITE
   --------------------------------------------------------------------
   procedure ERROR_SQLITE (db : sqlite_h.sqlite3_Access; func : String; query : String)
   is
      msg : String := "sqlite error while executing " & query &
        " in file  core-repo-operations.adb," & func & "(): " &
        SQLite.get_last_error_message (db);
   begin
      Event.emit_error (msg);
   end ERROR_SQLITE;


   --------------------------------------------------------------------
   --  run_transaction
   --------------------------------------------------------------------
   function run_transaction (db : sqlite_h.sqlite3_Access; query : String; savepoint : String)
                             return Boolean
   is
      function joinsql return String;
      function joinsql return String is
      begin
         if IsBlank (savepoint) then
            return query;
         else
            return query & " " & savepoint;
         end if;
      end joinsql;

      stmt : aliased sqlite_h.sqlite3_stmt_Access;
      func : constant String := "run_transaction";
   begin
      Event.emit_debug (4, "RDB: running '" & joinsql & "'");
      if SQLite.prepare_sql (db, joinsql, stmt'Access) then
         if not SQLite.step_through_statement (stmt => stmt, num_retries => 6) then
            ERROR_SQLITE (db, func, joinsql);
            SQLite.finalize_statement (stmt);
            return False;
         end if;
         SQLite.finalize_statement (stmt);
         return True;
      else
         ERROR_SQLITE (db, func, joinsql);
         return False;
      end if;
   end run_transaction;


   --------------------------------------------------------------------
   --  trax_begin
   --------------------------------------------------------------------
   function trax_begin (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "BEGIN IMMEDIATE TRANSACTION", "");
      else
         return run_transaction (db, "SAVEPOINT", savepoint);
      end if;
   end trax_begin;


   --------------------------------------------------------------------
   --  trax_commit
   --------------------------------------------------------------------
   function trax_commit (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "COMMIT TRANSACTION", "");
      else
         return run_transaction (db, "RELEASE SAVEPOINT", savepoint);
      end if;
   end trax_commit;


   --------------------------------------------------------------------
   --  trax_rollback
   --------------------------------------------------------------------
   function trax_rollback (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "ROLLBACK TRANSACTION", "");
      else
         return run_transaction (db, "ROLLBACK TO SAVEPOINT", savepoint);
      end if;
   end trax_rollback;


   --------------------------------------------------------------------
   --  close_all_open_repositories
   --------------------------------------------------------------------
   procedure close_all_open_repositories
   is
      procedure close (position : Repos_Priority_Crate.Cursor);
      procedure close (position : Repos_Priority_Crate.Cursor) is
      begin
         close_repository (Repos_Priority_Crate.Element (position).reponame, False);
      end close;
   begin
      repositories_open.Iterate (close'Access);
      repositories_open.Clear;
   end close_all_open_repositories;


   --------------------------------------------------------------------
   --  sqlite_filename
   --------------------------------------------------------------------
   function sqlite_filename (reponame : String) return String is
   begin
      --  TODO: Change extension back to ".sqlite" before releasing into production
      return reponame & ".sqlite.dev";
   end sqlite_filename;


   --------------------------------------------------------------------
   --  meta_filename
   --------------------------------------------------------------------
   function meta_filename (reponame : String) return String is
   begin
      return reponame & ".meta";
   end meta_filename;


   --------------------------------------------------------------------
   --  mode_sets_write_flag
   --------------------------------------------------------------------
--     function mode_sets_write_flag (mode : Pkgtypes.mode_t) return Boolean is
--     begin
--        case mode is
--           when 2#0010# | 2#0011# | 2#0110# | 2#0111# => return True;  -- 2,3,6,7
--           when 2#1010# | 2#1011# | 2#1110# | 2#1111# => return True;  -- 10,11,14,15
--           when others => return False;
--        end case;
--     end mode_sets_write_flag;


   --------------------------------------------------------------------
   --  get_pragma
   --------------------------------------------------------------------
   function get_pragma (db      : sqlite_h.sqlite3_Access;
                        sql     : String;
                        res     : out int64;
                        silence : Boolean) return Action_Result
   is
      stmt : aliased sqlite_h.sqlite3_stmt_Access;
      func : constant String := "get_pragma";
      nres : SQLite.sql_int64;
   begin
      nres := 0;
      Event.emit_debug (4, "repo-ops: executing pragma command '" & sql & "'");
      if not SQLite.prepare_sql (db, sql, stmt'Access) then
         if not silence then
            ERROR_SQLITE (db, func, sql);
         end if;
         return RESULT_FATAL;
      end if;

      if not SQLite.step_through_statement (stmt => stmt, num_retries => 6) then
         SQLite.finalize_statement (stmt);
         Event.emit_error ("repo-ops: failed to step through get_pragma()");
         return RESULT_FATAL;
      end if;

      nres := SQLite.retrieve_integer (stmt, 0);
      SQLite.finalize_statement (stmt);
      res := int64 (nres);

      return RESULT_OK;
   end get_pragma;


   --------------------------------------------------------------------
   --  user_version
   --------------------------------------------------------------------
   function user_version (db : sqlite_h.sqlite3_Access; reposcver : out Integer) return Boolean
   is
      sql    : constant String := "PRAGMA user_version";
      stmt   : aliased sqlite_h.sqlite3_stmt_Access;
      result : Boolean;
      invalid : constant Integer := -1;
   begin
      if not SQLite.prepare_sql (db, sql, stmt'Access) then
         ERROR_SQLITE (db, "user_version", sql);
         reposcver := invalid;
         return False;
      end if;
      if SQLite.step_through_statement (stmt) then
         result := True;
         reposcver := Integer (SQLite.retrieve_integer (stmt, 0));
      else
         reposcver := invalid;
         result := False;
      end if;

      SQLite.finalize_statement (stmt);
      return result;
   end user_version;


   --------------------------------------------------------------------
   --  repo_upgrade
   --------------------------------------------------------------------
   function repo_upgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      version  : Integer) return Action_Result
   is
      rc  : Action_Result := RESULT_OK;
   begin
      if version < Integer (grade_range'First) then
         Event.emit_error
           ("Repository " & reponame & " (version " & int2str (version) &
              ") is too old to upgrade. The oldest supported version is " &
              int2str (Integer (grade_range'First)));
         return RESULT_FATAL;
      end if;
      for ver in grade_range (version) .. grade_range'Last - 1 loop
         rc := repo_apply_upgrade (db       => db,
                                   reponame => reponame,
                                   version  => ver);
         exit when rc /= RESULT_OK;
      end loop;
      return rc;
   end repo_upgrade;


   --------------------------------------------------------------------
   --  check_version
   --------------------------------------------------------------------
   function check_version (db : sqlite_h.sqlite3_Access; reponame : String) return Action_Result
   is
      reposcver : Integer;
      repomajor : Integer;
      rc : Action_Result;
   begin
      if not user_version (db, reposcver) then
         return RESULT_FATAL;
      end if;

      --  If the local pkgng uses a repo schema behind that used to
      --  create the repo, we may still be able use it for reading
      --  (ie pkg install), but pkg repo can't do an incremental
      --  update unless the actual schema matches the compiled in
      --  schema version.
      --
      --  Use a major - minor version schema: as the user_version
      --  PRAGMA takes an integer version, encode this as MAJOR *
      --  1000 + MINOR.
      --
      --  So long as the major versions are the same, the local pkgng
      --  should be compatible with any repo created by a more recent
      --  pkgng, although it may need some modification of the repo
      --  schema

      repomajor := reposcver / 1000;

      if repomajor < REPO_SCHEMA_MAJOR then
         Event.emit_error
           ("Repo " & reponame & " (schema version " & int2str (reposcver) &
              " is too old -- the minimum requirement is schema " &
              int2str (REPO_SCHEMA_MAJOR * 1000));
         return RESULT_REPOSCHEMA;
      end if;

      if repomajor > REPO_SCHEMA_MAJOR then
         Event.emit_error
           ("Repo " & reponame & " (schema version " & int2str (reposcver) &
              " is too new -- the maximum requirement is schema " &
              int2str (((REPO_SCHEMA_MAJOR + 1) * 1000) - 1));
         return RESULT_REPOSCHEMA;
      end if;

      rc := RESULT_OK;

      declare
         RSV : constant Integer := Integer'Value (REPO_SCHEMA_VERSION);
      begin
         if reposcver < RSV then
            if SQLite.database_was_opened_readonly (db, "main") then
               Event.emit_error
                 ("Repo " & reponame & " needs schema upgrade from " & int2str (reposcver) &
                    " to " & REPO_SCHEMA_VERSION & "but it was opened as read-only");
               rc := RESULT_FATAL;
            else
               rc := repo_upgrade (db, reponame, reposcver);
            end if;
         end if;
      end;
      return rc;
   end check_version;


   --------------------------------------------------------------------
   --  open_repository
   --------------------------------------------------------------------
   function open_repository (reponame : String; readonly : Boolean) return Action_Result
   is
      --  Ensure that SQLite is initialized with syscall_override before calling this

      procedure open_database (key : Text; Element : in out A_repo);

      dbdirfd : Unix.File_Descriptor;
      result  : Action_Result;

      procedure open_database (key : Text; Element : in out A_repo)
      is
         repository : A_repo renames Element;
         errprefix  : constant String := "Repository " & reponame & " load error: ";

         fd         : Unix.File_Descriptor;
         filename   : constant String := meta_filename (reponame);
         dbfile     : constant String := sqlite_filename ("repo-" & reponame);
         flags      : constant Unix.T_Open_Flags := (RDONLY => True, others => False);
         success    : Action_Result;
         tmp        : Repo_metadata;
      begin
         fd := Unix.open_file (dirfd         => dbdirfd,
                               relative_path => filename,
                               flags         => flags);

         if Unix.file_connected (fd) then
            tmp := Repo.Meta.meta_load (fd, success);
            if success = RESULT_OK then
               repository.meta := tmp;
            else
               Event.emit_errno (errprefix & "openat", "dbdirfd, " & filename, Unix.errno);
               if Unix.close_file (fd) then
                  null;
               end if;
               return;
            end if;
            if not Unix.close_file (fd) then
               Event.emit_errno (errprefix & "close_file", "meta fd", Unix.errno);
               return;
            end if;
         else
            Event.emit_errno (errprefix & "open_file", filename, Unix.errno);
            return;
         end if;

         if not Unix.relative_file_readable (dbdirfd, dbfile) then
            Event.emit_error (dbfile & " is not readable");
            return;
         end if;

         --  main open routine
         declare
            use type sqlite_h.enum_error_types;
            opened : Boolean;
         begin
            if readonly then
               opened := SQLite.open_sqlite_database_readonly
                 (path => dbfile,
                  ppDB => repository.sqlite_handle'Access);
            else
               opened := SQLite.open_sqlite_database_readwrite
                 (path => dbfile,
                  ppDB => repository.sqlite_handle'Access);
            end if;
            if not opened then
               if SQLite.get_last_error_code (repository.sqlite_handle) =
                 sqlite_h.SQLITE_CORRUPT
               then
                  Event.emit_error
                    ("Database corrupt.  Are you running on NFS?  " &
                       "If so, ensure the locking mechanism is properly set up.");
               end if;
               Event.emit_errno (errprefix & "open sqlite", dbfile, Unix.errno);
               return;
            end if;
         end;

         --  Verify database is usable
         declare
            res_int64 : int64;
            sql       : constant String := "SELECT count(name) FROM sqlite_master " &
                                           "WHERE type='table' AND name='repodata'";
         begin
            if get_pragma (repository.sqlite_handle, sql, res_int64, False) /= RESULT_OK
            then
               Event.emit_errno (errprefix & "pragma", sql, Unix.errno);
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;

            if res_int64 /= 1 then
               Event.emit_error
                 ("Repository " & reponame &
                    " contains no repodata table, database must be recreated");
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;
         end;

         --  Check package site
         declare
            res_int64 : int64;
            url       : constant String := repo_url (repository);
            sql       : constant String := "select count(key) from repodata " &
                        "WHERE key = " & DQ ("packagesite") & " and value = " & SQ (url);
         begin
            if get_pragma (repository.sqlite_handle, sql, res_int64, False) /= RESULT_OK
            then
               Event.emit_errno (errprefix & "pragma", sql, Unix.errno);
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;

            if res_int64 /= 1 then
               Event.emit_error ("Repository " & reponame &
                                   " has a wrong packagesite, database must be recreated");
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;
         end;

         --  Check version
         if check_version (repository.sqlite_handle, reponame) /= RESULT_OK then
            Event.emit_error ("Repository " & reponame & " has an unsupported schema");
            Event.emit_error (" The database must be recreated.");
            SQLite.close_database (repository.sqlite_handle);
            if not readonly then
               begin
                  DIR.Delete_File (dbfile);
               exception
                  when others =>
                     Event.emit_error ("Failed to unlink " & dbfile & "!");
               end;
            end if;
            return;
         end if;

         --  Check digests format
--           declare
--              my_pkg : T_pkg_Access;
--              it     : Binary_sqlite.Iterator_Binary_Sqlite :=
--                pkg_repo_binary_query (db       => repository.sqlite_handle,
--                                       reponame => S_reponame,
--                                       pattern  => "",
--                                       match    => PkgDB.MATCH_ALL,
--                                       flags    => PKGDB_IT_FLAG_ONCE);
--           begin
--              if invalid_iterator (Base_Iterators (it)) then
--                 result := True;
--                 return;
--              end if;
--
--              if it.Next (my_pkg, PKG_LOAD_FLAG_BASIC) /= EPKG_OK then
--                 delete_pkg (my_pkg);
--                 result := True;
--                 return;
--              end if;
--
--              if IsBlank (my_pkg.digest) or else
--                not Checksum.pkg_checksum_is_valid (my_pkg.digest)
--              then
--                 Event.emit_error
--                   ("Repository " & reponame &
--                      " has an incompatible checksum format, database must be recreated");
--                 SQLite.close_database (repository.sqlite_handle);
--              else
--                 result := True;
--              end if;
--
--              delete_pkg (my_pkg);
--         end;
         result := RESULT_OK;
      end open_database;

   begin
      dbdirfd := Context.reveal_db_directory_fd;
      repositories.Update_Element (repositories.Find (SUS (reponame)), open_database'Access);
      return result;
   end open_repository;


   --------------------------------------------------------------------
   --  upgrade_info
   --------------------------------------------------------------------
   function upgrade_info (current_version : grade_range) return grade_info
   is
      info : grade_info;
   begin
      info.identifier := current_version;
      case current_version is
         when 2013 =>
            info.summary := SUS ("Remove full text search feature");
            info.query   := SUS ("DROP TABLE pkg_search;");
         when 2014 =>
            info.summary := SUS ("Fill in when 2015 comes");
            info.query   := SUS ("2015 Query");
      end case;
      return info;
   end upgrade_info;


   --------------------------------------------------------------------
   --  repo_set_version
   --------------------------------------------------------------------
   function repo_set_version
     (db : sqlite_h.sqlite3_Access;
      reposcver : grade_range) return Action_Result
   is
      ver : constant Integer := Integer (reposcver);
      sql : constant String :=  "PRAGMA user_version = " & int2str (ver) & ";";
      errmsg : Text;
   begin
      if SQLite.exec_sql (db, sql, errmsg) then
         return RESULT_OK;
      else
         Event.emit_error ("repo_set_version(): " & USS (errmsg));
         return RESULT_FATAL;
      end if;
   end repo_set_version;


   --------------------------------------------------------------------
   --  repo_apply_upgrade
   --------------------------------------------------------------------
   function repo_apply_upgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      version  : grade_range) return Action_Result
   is
      info      : grade_info := upgrade_info (version);
      in_trans  : Boolean := False;
      nextver   : grade_range := version + 1;
      rc        : Action_Result;
      sql       : constant String := USS (info.query);
      msg       : constant String := USS (info.summary);
      savepoint : constant String := "SCHEMA";
      errmsg    : Text;
   begin
      --  Begin Transaction
      if trax_begin (db, savepoint) then
         rc := RESULT_OK;
         in_trans := True;

         --  Apply change
         Event.emit_debug (3, "Repo mod: " & msg);
         Event.emit_debug (4, "Pkgdb: running '" & sql & "'");
         if not SQLite.exec_sql (db, sql, errmsg) then
            Event.emit_error ("sqlite: " & USS (errmsg));
            rc := RESULT_FATAL;
         end if;
      else
         rc := RESULT_FATAL;
      end if;

      --  update repo user_version
      if rc = RESULT_OK then
         rc := repo_set_version (db, nextver);
      end if;

      --  commit or rollback
      if in_trans then
         if rc = RESULT_OK then
            if not trax_commit (db, savepoint) then
               rc := RESULT_FATAL;
            end if;
         else
            if trax_rollback (db, savepoint) then
               null;
            end if;
         end if;
      end if;

      if rc = RESULT_OK then
         Event.emit_notice
           ("Repo '" & reponame & "' upgrade schema " &
                 int2str (Integer (version)) & " to " &
                 int2str (Integer (nextver)) & ": " & msg);
      end if;
      return rc;
   end repo_apply_upgrade;

end Core.Repo.Operations;
