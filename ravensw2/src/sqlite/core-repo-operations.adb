--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Directories;

with Core.Event;
with Core.Context;
with Core.Unix;
with Core.Repo.Meta;
with Core.Repo.Operations.Schema;
with Core.VFS;
with Core.Database.CustomCmds;
with Core.CommonSQL;
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
         Schema.prstmt_finalize (repository.sqlite_handle);
         SQLite.close_database (repository.sqlite_handle);
         repository.sqlite_handle := null;
      end close_database;
   begin
      if repositories.Contains (reponame) then

         if not SQLite.db_connected (repositories.Element (reponame).sqlite_handle) then
            return;
         end if;

         if commit then
            if not CommonSQL.transaction_commit
              (repositories.Element (reponame).sqlite_handle, "")
            then
               Event.emit_error ("close_repository(): Failed to commit transaction");
               return;
            end if;
         end if;

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

         --  Upgrade as necessary
         declare
            upres : Action_Result;
         begin
            upres := Schema.repo_upgrade (repository.sqlite_handle, reponame);
            case upres is
               when RESULT_UPTODATE => null;
               when RESULT_OK       => null;
               when RESULT_REPOSCHEMA
                  | RESULT_FATAL =>
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
               when others =>
                  null;  --  shouldn't happen
            end case;
         end;

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
   --  initialize_repository
   --------------------------------------------------------------------
   function initialize_repository (reponame : Text) return Action_Result
   is
      procedure make_it_so (key : Text; Element : in out A_repo);

      result : Action_Result;

      procedure make_it_so (key : Text; Element : in out A_repo)
      is
--         repository : A_repo renames Element;
         db : sqlite_h.sqlite3_Access renames Element.sqlite_handle;
         onward : Boolean := True;
      begin
         --  Impossible to fail
         Database.CustomCmds.define_file_exists (db);

         if CommonSQL.exec (db, "PRAGMA synchronous=default") /= RESULT_OK then
            onward := False;
         end if;

         if onward then
            if CommonSQL.exec (db, "PRAGMA foreign_keys=on") /= RESULT_OK then
               onward := False;
            end if;
         end if;

         if onward then
            Database.CustomCmds.define_six_functions (db);
            if Schema.prstmt_initialize (db) /= RESULT_OK then
               onward := False;
            end if;
         end if;

         case onward is
            when True  => result := RESULT_OK;
            when False => result := RESULT_FATAL;
         end case;
      end make_it_so;

   begin
      if repositories.Contains (reponame) then
         repositories.Update_Element (repositories.Find (reponame), make_it_so'Access);
         return result;
      else
         raise invalid_repo_name;
      end if;
   end initialize_repository;


end Core.Repo.Operations;
