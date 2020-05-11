--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Interfaces.C.Strings;

with Core.Database.CustomCmds;
with Core.Strings;
with Core.Context;
with Core.Config;
with Core.Event;
with Core.Repo;
with Core.Unix;
with SQLite;

use Core.Strings;

package body Core.Database.Operations is

   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package ICS renames Interfaces.C.Strings;
   package CUS renames Core.Database.CustomCmds;

   --------------------------------------------------------------------
   --  ERROR_SQLITE
   --------------------------------------------------------------------
   procedure ERROR_SQLITE (db : sqlite_h.sqlite3_Access; func : String; query : String)
   is
      msg : String := "sqlite error while executing " & query &
        " in file core-database-operations.adb," & func & "(): " &
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
   --  transaction_begin
   --------------------------------------------------------------------
   function transaction_begin (db : RDB_Connection; savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db.sqlite, "BEGIN IMMEDIATE TRANSACTION", "");
      else
         return run_transaction (db.sqlite, "SAVEPOINT", savepoint);
      end if;
   end transaction_begin;


   --------------------------------------------------------------------
   --  transaction_commit
   --------------------------------------------------------------------
   function transaction_commit (db : RDB_Connection; savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db.sqlite, "COMMIT TRANSACTION", "");
      else
         return run_transaction (db.sqlite, "RELEASE SAVEPOINT", savepoint);
      end if;
   end transaction_commit;


   --------------------------------------------------------------------
   --  transaction_rollback
   --------------------------------------------------------------------
   function transaction_rollback (db : RDB_Connection; savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db.sqlite, "ROLLBACK TRANSACTION", "");
      else
         return run_transaction (db.sqlite, "ROLLBACK TO SAVEPOINT", savepoint);
      end if;
   end transaction_rollback;


   --------------------------------------------------------------------
   --  rdb_open_all
   --------------------------------------------------------------------
   function rdb_open_all (db       : in out RDB_Connection;
                          dbtype   : RDB_Source)
                          return Action_Result
   is
   begin
      return rdb_open (db, dbtype, "");
   end rdb_open_all;


   --------------------------------------------------------------------
   --  rdb_open
   --------------------------------------------------------------------
   function rdb_open (db : in out RDB_Connection;
                      dbtype : RDB_Source;
                      reponame : String)
                      return Action_Result
   is
      use type sqlite_h.enum_error_types;
      func   : constant String := "rdb_open_all()";
      dbdir  : constant String := Config.configuration_value (Config.dbdir);
      key    : constant String := config.get_ci_key (Config.dbdir);
      dirfd  : Unix.File_Descriptor;
      create : Boolean := False;
      result : Boolean;
   begin
      if not SQLite.db_connected (db.sqlite) then
         --  database is already open, just load another repository and exit
         --  return rdb_open_remote (db, dbtype, reponame);

         --  Create db directory if it doesn't already exist
         if DIR.Exists (dbdir) then
            case DIR.Kind (dbdir) is
               when DIR.Directory => null;
               when others =>
                  Event.emit_error (func & ": " & key & " exists but is not a directory");
                  return RESULT_FATAL;
            end case;
         else
            begin
               DIR.Create_Path (dbdir);
            exception
               when others =>
                  Event.emit_error (func & ": Failed to create " & key & " directory");
                  return RESULT_FATAL;
            end;
         end if;

         dirfd := Context.reveal_db_directory_fd;
         if not Unix.file_connected (dirfd) then
            Event.emit_error
              (func & ": Failed to open " & key & " directory as a file descriptor");
            return RESULT_FATAL;
         end if;

         if not Unix.relative_file_readable (dirfd, "local.sqlite") then
            if DIR.Exists (dbdir & "/local.sqlite") then
               --  db file exists but we can't write to it, fail
               Event.emit_no_local_db;
               rdb_close (db);
               return RESULT_ENODB;
            elsif not Unix.relative_file_writable (dirfd, ".") then
               --  We need to create db file but we can't even write to the containing
               --  directory, so fail
               Event.emit_no_local_db;
               rdb_close (db);
               return RESULT_ENODB;
            else
               create := True;
            end if;
         end if;

         result := SQLite.initialize_sqlite;
         SQLite.rdb_syscall_overload;

         if not SQLite.open_sqlite_database_readwrite ("/local.sqlite", db.sqlite'Access) then
            ERROR_SQLITE (db.sqlite, func, "sqlite open");
            if SQLite.get_last_error_code (db.sqlite) = sqlite_h.SQLITE_CORRUPT then
               Event.emit_error
                 (func & ": Database corrupt.  Are you running on NFS?  " &
                    "If so, ensure the locking mechanism is properly set up.");
            end if;
            rdb_close (db);
            return RESULT_FATAL;
         end if;

         --  Wait up to 5 seconds if database is busy
         declare
            use type IC.int;
            res : IC.int;
         begin
            res := sqlite_h.sqlite3_busy_timeout (db.sqlite, IC.int (5000));
            if res /= 0 then
               Event.emit_error (func & ": Failed to set busy timeout");
            end if;
         end;

         --  The database file is blank when create is set, so we have to initialize it
         if create and then
           pkgdb_init (db.sqlite) /= RESULT_OK
         then
            rdb_close (db);
            return RESULT_FATAL;
         end if;

         --  Create custom functions
         declare
            use type IC.int;
            res : IC.int;
         begin
            res := CUS.sqlcmd_init (db.sqlite, null, null);
            if res /= 0 then
               Event.emit_error (func & ": Failed to add custom SQL functions");
               rdb_close (db);
               return RESULT_FATAL;
            end if;
         end;

         if rdb_upgrade (db) /= RESULT_OK then
            --  rdb_upgrade() emits error events; we don't need to add more
            rdb_close (db);
            return RESULT_FATAL;
         end if;

         --  allow foreign key option which will allow to have
         --  clean support for reinstalling
         declare
            msg : Text;
            sql : constant String := "PRAGMA foreign_keys = ON";
         begin
            if not SQLite.exec_sql (db.sqlite, sql, msg) then
               ERROR_SQLITE (db.sqlite, func, sql);
               rdb_close (db);
               return RESULT_FATAL;
            end if;
         end;
      end if;   --  END CONNECTION BLOCK

      declare
         result : Action_Result;
      begin
         result := rdb_open_remote (db, dbtype, reponame);
         if result /= RESULT_OK then
            rdb_close (db);
            return result;
         end if;
      end;

      if prstmt_initialize (db) /= RESULT_OK then
         Event.emit_error (func & ": Failed to initialize prepared statements");
         rdb_close (db);
         return RESULT_FATAL;
      end if;

      if Config.configuration_value (Config.sqlite_profile) then
         Event.emit_debug (1, "raven database profiling is enabled");
         SQLite.set_sqlite_profile (db.sqlite, rdb_profile_callback'Access);
      end if;

      return RESULT_OK;
   end rdb_open;


   --------------------------------------------------------------------
   --  rdb_open_remote
   --------------------------------------------------------------------
   function rdb_open_remote (db       : in out RDB_Connection;
                             dbtype   : RDB_Source;
                             reponame : String)
                             return Action_Result
   is
      --  The calling procedure will close db upon error
      ret : Action_Result;
   begin
      case dbtype is
         when RDB_REMOTE       => null;
         when RDB_MAYBE_REMOTE => null;
         when RDB_DEFAULT      => return RESULT_OK;
      end case;

      if not IsBlank (reponame) then
         if Repo.repository_is_active (reponame) then
            ret := rdb_open_repository (db, reponame);
            if ret /= RESULT_OK then
               Event.emit_error ("Failed to open repository " & reponame);
            end if;
            return ret;
         else
            Event.emit_error ("Repository " & reponame & " is not active or does not exist");
            return RESULT_FATAL;
         end if;
      elsif Repo.count_of_active_repositories > 0 then
         declare
            list  : String := Repo.joined_priority_order;
            num   : Natural := count_char (list, LAT.LF) + 1;
            delim : String (1 .. 1) := (others => LAT.LF);
         begin
            for x in 1 .. num loop
               declare
                  rname : String := specific_field (list, x, delim);
               begin
                  if Repo.repository_is_active (rname) then
                     ret := rdb_open_repository (db, rname);
                     if ret /= RESULT_OK then
                        Event.emit_error ("Failed to open repository " & rname);
                     end if;
                     return ret;
                  end if;
               end;
            end loop;
         end;
      else
         Event.emit_error ("No active remote repositories configured");
         return RESULT_FATAL;
      end if;
      return RESULT_OK;
   end rdb_open_remote;

   --------------------------------------------------------------------
   --  prstmt_finalize
   --------------------------------------------------------------------
   procedure prstmt_finalize (db : in out RDB_Connection) is
   begin
      for S in sql_prstmt_index'Range loop
         SQLite.finalize_statement (sql_prepared_statements (S));
         sql_prepared_statements (S) := null;
      end loop;
      db.prstmt_initialized := False;
   end prstmt_finalize;


   --------------------------------------------------------------------
   --  prstmt_initialize
   --------------------------------------------------------------------
   function prstmt_initialize (db : in out RDB_Connection) return Action_Result is
   begin
      if not db.prstmt_initialized then
         for S in sql_prstmt_index'Range loop
            Event.emit_debug (4, "Pkgdb: preparing statement '" & prstmt_text_sql (S) & "'");
            if not SQLite.prepare_sql (pDB    => db.sqlite,
                                       sql    => prstmt_text_sql (S),
                                       ppStmt => sql_prepared_statements (S)'Access)
            then
               ERROR_SQLITE (db.sqlite, "prstmt_initialize", prstmt_text_sql (S));
               return RESULT_FATAL;
            end if;
            db.prstmt_initialized := True;
         end loop;
      end if;
      return RESULT_OK;
   end prstmt_initialize;


   --------------------------------------------------------------------
   --  rdb_profile_callback
   --------------------------------------------------------------------
   function rdb_profile_callback
     (trace_type : IC.unsigned;
      ud   : sqlite_h.Void_Ptr;
      stmt : sqlite_h.Void_Ptr;
      x    : sqlite_h.Void_Ptr) return IC.int
   is
      use type SQLite.sql_int64;
      nsec        : SQLite.sql_int64;
      nsec_Access : access SQLite.sql_int64;
      stmt_Access : sqlite_h.sqlite3_stmt_Access;

      for nsec_Access'Address use x;
      pragma Import (Ada, nsec_Access);

      for stmt_Access'Address use stmt;
      pragma Import (Ada, stmt_Access);
   begin
      --  According to sqlite3 documentation, nsec has milliseconds accuracy
      nsec := nsec_Access.all / 1_000_000;
      if nsec > 0 then
         Event.emit_debug (1, "Sqlite request " & SQLite.get_sql (stmt_Access) &
                             " was executed in " & int2str (Integer (nsec)) & " milliseconds");
      end if;
      return IC.int'Val (0);
   end rdb_profile_callback;


   --------------------------------------------------------------------
   --  rdb_close
   --------------------------------------------------------------------
   procedure rdb_close   (db : in out RDB_Connection)
   is
      use type sqlite_h.sqlite3_Access;

      procedure close (position : Text_Jar.Cursor);
      procedure close (position : Text_Jar.Cursor)
      is
         result   : Boolean;
         reponame : Text renames Text_Jar.Element (position);
         variant  : repo_ops_variant := Config.repositories.Element (reponame).ops_variant;
      begin
         result := Repo_Operations.Ops (variant).all.repo_close (reponame, False);
      end close;

   begin
      if db.prstmt_initialized then
         prstmt_finalize (db);
      end if;
      if db.sqlite /= null then
         db.repos.Iterate (close'Access);
         db.repos.Clear;
         SQLite.close_database (db.sqlite);
         db.sqlite := null;
      end if;
      SQLite.shutdown_sqlite;
   end rdb_close;

end Core.Database.Operations;
