--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Interfaces.C.Strings;

with Core.CommonSQL;
with Core.Database.CustomCmds;
with Core.Database.Operations.Schema;
with Core.Repo.Operations;
with Core.Strings;
with Core.Context;
with Core.Config;
with Core.Event;
with Core.Repo;
with SQLite;

use Core.Strings;

package body Core.Database.Operations is

   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package ICS renames Interfaces.C.Strings;
   package CUS renames Core.Database.CustomCmds;
   package ROP renames Core.Repo.Operations;

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
      func   : constant String := "rdb_open()";
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

         if not Unix.relative_file_readable (dirfd, local_ravensw_db) then
            if DIR.Exists (dbdir & "/" & local_ravensw_db) then
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

         if not SQLite.open_sqlite_database_readwrite ("/" & local_ravensw_db, db.sqlite'Access)
         then
            CommonSQL.ERROR_SQLITE (db      => db.sqlite,
                                    srcfile => internal_srcfile,
                                    func    => func,
                                    query   => "sqlite open");
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
           Schema.import_schema_34 (db.sqlite) /= RESULT_OK
         then
            rdb_close (db);
            return RESULT_FATAL;
         end if;

         --  Create custom functions
         CUS.define_six_functions (db.sqlite);

         if Schema.rdb_upgrade (db) /= RESULT_OK then
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
               CommonSQL.ERROR_SQLITE (db.sqlite, internal_srcfile, func, sql);
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

      if Schema.prstmt_initialize (db) /= RESULT_OK then
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
            ret := ROP.open_repository (reponame, True);
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
                     ret := ROP.open_repository (rname, True);
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
   begin
      if db.prstmt_initialized then
         Schema.prstmt_finalize (db);
      end if;
      if db.sqlite /= null then
         ROP.close_all_open_repositories;
         SQLite.close_database (db.sqlite);
         db.sqlite := null;
      end if;
      SQLite.shutdown_sqlite;
   end rdb_close;


   --------------------------------------------------------------------
   --  rdb_obtain_lock
   --------------------------------------------------------------------
   function rdb_obtain_lock
     (db       : in out RDB_Connection;
      locktype : RDB_Lock_Type) return Boolean
   is
   begin
      case locktype is
         when RDB_LOCK_READONLY  =>
            if not Config.configuration_value (config.read_lock) then
               return True;
            end if;
            Event.emit_debug (1, "want to get a read only lock on a database");
            return
              rdb_try_lock
                (db        => db,
                 lock_sql  => "UPDATE pkg_lock SET read=read+1 WHERE exclusive=0;",
                 lock_type => locktype,
                 upgrade   => False);

         when RDB_LOCK_ADVISORY  =>
            Event.emit_debug (1, "want to get an advisory lock on a database");
            return
              rdb_try_lock
                (db        => db,
                 lock_sql  => "UPDATE pkg_lock SET advisory=1 WHERE exclusive=0 AND advisory=0;",
                 lock_type => locktype,
                 upgrade   => False);

         when RDB_LOCK_EXCLUSIVE =>
            Event.emit_debug (1, "want to get an exclusive lock on a database");
            return
              rdb_try_lock
                (db        => db,
                 lock_sql  => "UPDATE pkg_lock SET exclusive=1 " &
                              "WHERE exclusive=0 AND advisory=0 AND read=0;",
                 lock_type => locktype,
                 upgrade   => False);

      end case;
   end rdb_obtain_lock;


   --------------------------------------------------------------------
   --  rdb_release_lock
   --------------------------------------------------------------------
   function rdb_release_lock
     (db       : in out RDB_Connection;
      locktype : RDB_Lock_Type) return Boolean
   is
      result : Action_Result;
   begin
      case locktype is
         when RDB_LOCK_READONLY  =>
            if not Config.configuration_value (config.read_lock) then
               return True;
            end if;
            Event.emit_debug (1, "release a read only lock on a database");
            result := CommonSQL.exec
              (db.sqlite, "UPDATE pkg_lock SET read=read-1 WHERE read>0;");

         when RDB_LOCK_ADVISORY  =>
            Event.emit_debug (1, "release an advisory lock on a database");
            result := CommonSQL.exec
              (db.sqlite, "UPDATE pkg_lock SET advisory=0 WHERE advisory=1;");

         when RDB_LOCK_EXCLUSIVE =>
            Event.emit_debug (1, "release an exclusive lock on a database");
            result := CommonSQL.exec
              (db.sqlite, "UPDATE pkg_lock SET exclusive=0 WHERE exclusive=1;");
      end case;
      if result /= RESULT_OK then
         return False;
      end if;

      if SQLite.get_number_of_changes (db.sqlite) = 0 then
         return True;
      end if;
      return rdb_remove_lock_pid (db, Unix.getpid);
   end rdb_release_lock;


   --------------------------------------------------------------------
   --  rdb_reset_lock
   --------------------------------------------------------------------
   function rdb_reset_lock (db : in out RDB_Connection) return Boolean
   is
      res : Action_Result;
   begin
      res := CommonSQL.exec (db.sqlite, "UPDATE pkg_lock SET exclusive=0, advisory=0, read=0;");
      return (res = RESULT_OK);
   end rdb_reset_lock;


   --------------------------------------------------------------------
   --  rdb_write_lock_pid
   --------------------------------------------------------------------
   function rdb_write_lock_pid (db : in out RDB_Connection) return Action_Result
   is
      lock_pid_sql : constant String := "INSERT INTO pkg_lock_pid VALUES (?1);";
      stmt         : aliased sqlite_h.sqlite3_stmt_Access;
   begin
      if SQLite.prepare_sql (pDB    => db.sqlite,
                             sql    => lock_pid_sql,
                             ppStmt => stmt'Access)
      then
         SQLite.bind_integer (stmt, 1, SQLite.sql_int64 (Unix.getpid));
         if not SQLite.step_to_completion (stmt) then
            CommonSQL.ERROR_SQLITE
              (db.sqlite, internal_srcfile, "rdb_write_lock_pid (step)", lock_pid_sql);
            SQLite.finalize_statement (stmt);
            return RESULT_FATAL;
         end if;
         SQLite.finalize_statement (stmt);
         return RESULT_OK;
      else
         CommonSQL.ERROR_SQLITE
           (db.sqlite, internal_srcfile, "rdb_write_lock_pid (prep)", lock_pid_sql);
         return RESULT_FATAL;
      end if;
   end rdb_write_lock_pid;


   --------------------------------------------------------------------
   --  rdb_check_lock_pid
   --------------------------------------------------------------------
   function rdb_check_lock_pid (db : in out RDB_Connection) return Action_Result
   is
      use type Unix.Process_ID;

      query : constant String := "SELECT pid FROM pkg_lock_pid;";
      stmt  : aliased sqlite_h.sqlite3_stmt_Access;
      lpid  : Unix.Process_ID;
      pid   : Unix.Process_ID;
      found : Integer := 0;
   begin
      if SQLite.prepare_sql (pDB    => db.sqlite,
                             sql    => query,
                             ppStmt => stmt'Access)
      then
         lpid := Unix.getpid;

         loop
            exit when not SQLite.step_to_another_row (stmt);

            pid := Unix.Process_ID (SQLite.retrieve_integer (stmt, 0));
            if pid /= lpid then
               if Unix.kill (pid) then
                  Event.emit_notice ("process with pid" & pid'Img & " still holds the lock");
                  found := found + 1;
               else
                  Event.emit_debug
                    (1, "found stale pid" & pid'Img & " in lock database, my pid is:" & lpid'Img);
                  if not rdb_remove_lock_pid (db, pid) then
                     SQLite.finalize_statement (stmt);
                     return RESULT_FATAL;
                  end if;
               end if;
            end if;
         end loop;
      else
         CommonSQL.ERROR_SQLITE (db.sqlite, internal_srcfile, "rdb_check_lock_pid (prep)", query);
         return RESULT_FATAL;
      end if;
      SQLite.finalize_statement (stmt);

      if found = 0 then
         return RESULT_END;
      else
         return RESULT_OK;
      end if;
   end rdb_check_lock_pid;


   --------------------------------------------------------------------
   --  rdb_remove_lock_pid
   --------------------------------------------------------------------
   function rdb_remove_lock_pid
     (db  : in out RDB_Connection;
      pid : Unix.Process_ID) return Boolean
   is
      lock_pid_sql : constant String := "DELETE FROM pkg_lock_pid WHERE pid = ?1;";
      stmt         : aliased sqlite_h.sqlite3_stmt_Access;
   begin
      if SQLite.prepare_sql (pDB    => db.sqlite,
                             sql    => lock_pid_sql,
                             ppStmt => stmt'Access)
      then
         SQLite.bind_integer (stmt, 1, SQLite.sql_int64 (pid));
         if not SQLite.step_to_completion (stmt) then
            CommonSQL.ERROR_SQLITE
              (db.sqlite, internal_srcfile, "rdb_remove_lock_pid (step)", lock_pid_sql);
            SQLite.finalize_statement (stmt);
            return False;
         end if;
         SQLite.finalize_statement (stmt);
         return True;
      else
         CommonSQL.ERROR_SQLITE
           (db.sqlite, internal_srcfile, "rdb_remove_lock_pid (prep)", lock_pid_sql);
         return False;
      end if;
   end rdb_remove_lock_pid;


   --------------------------------------------------------------------
   --  rdb_try_lock
   --------------------------------------------------------------------
   function rdb_try_lock
     (db        : in out RDB_Connection;
      lock_sql  : String;
      lock_type : RDB_Lock_Type;
      upgrade   : Boolean) return Boolean
   is
      reset_lock_sql : constant String :=
        "DELETE FROM pkg_lock; INSERT INTO pkg_lock VALUES (0,0,0);";

      max_retries  : int64;
      timeout_secs : int64;
      retrys       : int64 := 0;
      retcode      : Action_Result := RESULT_END;
      msg          : Text;
   begin
      max_retries  := Config.configuration_value (Config.lock_retries);
      timeout_secs := Config.configuration_value (Config.lock_wait);
      loop
         exit when retrys > max_retries;
         if CommonSQL.exec (db.sqlite, lock_sql) /= RESULT_OK then
            retcode := RESULT_FATAL;
            exit;
         end if;

         retcode := RESULT_END;
         if SQLite.get_number_of_changes (db.sqlite) = 0 then
            if rdb_check_lock_pid (db) = RESULT_END then
               --  No live processes found, so we can safely reset lock
               Event.emit_debug (1, "no concurrent processes found, cleanup the lock");
               if not rdb_reset_lock (db) then
                  retcode := RESULT_FATAL;
                  exit;
               end if;

               if upgrade then
                  --  In case of upgrade we should obtain a lock from the beginning
                  --  hence switch upgrade to retain
                  if rdb_remove_lock_pid (db, Unix.getpid) then
                     if rdb_obtain_lock (db, lock_type) then
                        retcode := RESULT_OK;
                     else
                        retcode := RESULT_FATAL;
                     end if;
                  else
                     retcode := RESULT_FATAL;
                  end if;
               else
                  --  We might have inconsistent db, or some strange issue, so
                  --  just insert new record and go forward
                  if rdb_remove_lock_pid (db, Unix.getpid) then
                     retcode := CommonSQL.exec (db.sqlite, lock_sql);
                  else
                     retcode := RESULT_FATAL;
                  end if;
               end if;
               exit;
            elsif max_retries > 0 then
               Event.emit_debug
                 (1, "waiting for database lock for a maximum of" & max_retries'Img &
                    " retries, next try in" & timeout_secs'Img & " seconds");
               delay Duration (timeout_secs);
            else
               exit;
            end if;
         elsif not upgrade then
            retcode := rdb_write_lock_pid (db);
            exit;
         else
            retcode := RESULT_OK;
            exit;
         end if;

         retrys := retrys + 1;
      end loop;
      return (retcode = RESULT_OK);
   end rdb_try_lock;


   --------------------------------------------------------------------
   --  database_access
   --------------------------------------------------------------------
   function database_access (mode  : RDB_Mode_Flags; dtype : RDB_Type) return Action_Result
   is
      --  This will return one of:
      --
      --  RESULT_ENODB:
      --    A database doesn't exist and we don't want to create
      --    it, or dbdir doesn't exist
      --
      --  RESULT_INSECURE:
      --    The dbfile or one of the directories in the
      --    path to it are writable by other than root or
      --    (if $INSTALL_AS_USER is set) the current euid and egid
      --
      --  RESULT_ENOACCESS:
      --    we don't have privileges to read or write
      --
      --  RESULT_FATAL:
      --    Couldn't determine the answer for other reason,
      --    like configuration screwed up, invalid argument values,
      --    read-only filesystem, etc.
      --
      --  RESULT_OK:
      --    We can go ahead

      db_dir : String := Config.configuration_value (Config.dbdir);
      retval : Action_Result := RESULT_OK;
      RW     : constant RDB_Mode_Flags := (RDB_MODE_READ or RDB_MODE_WRITE);
   begin
      if mode = RDB_Mode_Flags (0) then
         return RESULT_FATAL;
      end if;

      --  Test the enclosing directory: if we're going to create the
      --  DB, then we need read and write permissions on the dir.
      --  Otherwise, just test for read access

      if (mode and RDB_MODE_CREATE) > 0 then
         retval := check_access (RW, db_dir, "");
      else
         retval := check_access (RDB_MODE_READ, db_dir, "");
      end if;
      if retval /= RESULT_OK then
         return retval;
      end if;

      case dtype is
         when RDB_DB_LOCAL =>
            --  Test local.sqlite, if required
            return check_access (mode, db_dir, local_ravensw_db);
         when RDB_DB_REPO =>
            if Repo.count_of_active_repositories > 0 then
               declare
                  list  : String := Repo.joined_priority_order;
                  num   : Natural := count_char (list, LAT.LF) + 1;
                  delim : String (1 .. 1) := (others => LAT.LF);
               begin
                  for x in 1 .. num loop
                     declare
                        rname : String := specific_field (list, x, delim);
                     begin
                        retval := Repo.Operations.check_repository_access (rname, mode);
                        if retval = RESULT_ENODB and then
                          (mode and RDB_MODE_READ) > 0
                        then
                           Event.emit_error
                             ("Repository " & rname & " missing, " &
                                SQ (progname & " update") & " command required");
                        end if;
                        if retval /= RESULT_OK then
                           exit;
                        end if;
                     end;
                  end loop;
               end;
            end if;
            return retval;
      end case;
   end database_access;


end Core.Database.Operations;
