--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Interfaces.C.Strings;
with System;

with Core.Event;
with Core.Strings;
with SQLite;

use Core.Strings;

package body Core.CommonSQL is

   package ICS renames Interfaces.C.Strings;
   package IC  renames Interfaces.C;

   --------------------------------------------------------------------
   --  run_transaction
   --------------------------------------------------------------------
   function run_transaction (db        : sqlite_h.sqlite3_Access;
                             srcfile   : String;
                             func      : String;
                             query     : String;
                             savepoint : String)
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
   begin
      Event.emit_debug (4, "RDB: running " & SQ (joinsql));
      if SQLite.prepare_sql (db, joinsql, stmt'Access) then
         if not SQLite.step_through_statement (stmt => stmt, num_retries => 6) then
            ERROR_SQLITE (db, srcfile, func, joinsql);
            SQLite.finalize_statement (stmt);
            return False;
         end if;
         SQLite.finalize_statement (stmt);
         return True;
      else
         ERROR_SQLITE (db, srcfile, func, joinsql);
         return False;
      end if;
   end run_transaction;


   --------------------------------------------------------------------
   --  transaction_begin
   --------------------------------------------------------------------
   function transaction_begin (db        : sqlite_h.sqlite3_Access;
                               srcfile   : String;
                               func      : String;
                               savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, srcfile, func, "BEGIN IMMEDIATE TRANSACTION", "");
      else
         return run_transaction (db, srcfile, func, "SAVEPOINT", savepoint);
      end if;
   end transaction_begin;


   --------------------------------------------------------------------
   --  transaction_commit
   --------------------------------------------------------------------
   function transaction_commit (db        : sqlite_h.sqlite3_Access;
                                srcfile   : String;
                                func      : String;
                                savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, srcfile, func, "COMMIT TRANSACTION", "");
      else
         return run_transaction (db, srcfile, func, "RELEASE SAVEPOINT", savepoint);
      end if;
   end transaction_commit;


   --------------------------------------------------------------------
   --  transaction_rollback
   --------------------------------------------------------------------
   function transaction_rollback (db        : sqlite_h.sqlite3_Access;
                                  srcfile   : String;
                                  func      : String;
                                  savepoint : String) return Boolean
   is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, srcfile, func, "ROLLBACK TRANSACTION", "");
      else
         return run_transaction (db, srcfile, func, "ROLLBACK TO SAVEPOINT", savepoint);
      end if;
   end transaction_rollback;


   --------------------------------------------------------------------
   --  ERROR_SQLITE
   --------------------------------------------------------------------
   procedure ERROR_SQLITE (db      : sqlite_h.sqlite3_Access;
                           srcfile : String;
                           func    : String;
                           query   : String)
   is
      msg : String := "sqlite error while executing " & query &
        " in file " & srcfile & ", " & func & ": " &
        SQLite.get_last_error_message (db);
   begin
      Event.emit_error (msg);
   end ERROR_SQLITE;


   --------------------------------------------------------------------
   --  get_pragma
   --------------------------------------------------------------------
   function get_pragma (db      : sqlite_h.sqlite3_Access;
                        srcfile : String;
                        func    : String;
                        sql     : String;
                        res     : out int64;
                        silence : Boolean) return Action_Result
   is
      stmt : aliased sqlite_h.sqlite3_stmt_Access;
      nres : SQLite.sql_int64;
   begin
      nres := 0;
      Event.emit_debug (4, "executing pragma command " & SQ (sql));
      if not SQLite.prepare_sql (db, sql, stmt'Access) then
         if not silence then
            ERROR_SQLITE (db, srcfile, func, sql);
         end if;
         return RESULT_FATAL;
      end if;

      if not SQLite.step_through_statement (stmt => stmt, num_retries => 6) then
         SQLite.finalize_statement (stmt);
         Event.emit_error ("failed to step through get_pragma()");
         return RESULT_FATAL;
      end if;

      nres := SQLite.retrieve_integer (stmt, 0);
      SQLite.finalize_statement (stmt);
      res := int64 (nres);

      return RESULT_OK;
   end get_pragma;


   --------------------------------------------------------------------
   --  exec
   --------------------------------------------------------------------
   function exec (db : sqlite_h.sqlite3_Access; sql : String) return Action_Result
   is
      msg : Text;
   begin
      Event.emit_debug (4, "executing " & SQ (sql));
      if SQLite.exec_sql (db, sql, msg) then
         return RESULT_OK;
      else
         Event.emit_error ("CommonSQL.exec() error: " & USS (msg));
         return RESULT_FATAL;
      end if;
   end exec;


   --------------------------------------------------------------------
   --  create_function
   --------------------------------------------------------------------
   procedure create_function (db    : sqlite_h.sqlite3_Access;
                              name  : String;
                              nargs : Natural;
                              cb    : sqlite_h.cb_xFuncStep)
   is
      use type IC.int;
      flags : constant IC.int := sqlite_h.SQLITE_ANY + sqlite_h.SQLITE_DETERMINISTIC;
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_create_function
        (db            => db,
         zFunctionName => ICS.New_String (name),
         nArg          => IC.int (nargs),
         eTextRep      => flags,
         pApp          => System.Null_Address,
         xFunc         => cb,
         xStep         => null,
         xFinal        => null);
   end create_function;


end Core.CommonSQL;
