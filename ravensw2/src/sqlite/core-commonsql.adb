--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Event;
with Core.Strings;
with SQLite;

use Core.Strings;

package body Core.CommonSQL is

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
   function transaction_begin (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "BEGIN IMMEDIATE TRANSACTION", "");
      else
         return run_transaction (db, "SAVEPOINT", savepoint);
      end if;
   end transaction_begin;


   --------------------------------------------------------------------
   --  transaction_commit
   --------------------------------------------------------------------
   function transaction_commit (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "COMMIT TRANSACTION", "");
      else
         return run_transaction (db, "RELEASE SAVEPOINT", savepoint);
      end if;
   end transaction_commit;


   --------------------------------------------------------------------
   --  transaction_rollback
   --------------------------------------------------------------------
   function transaction_rollback (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean
   is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "ROLLBACK TRANSACTION", "");
      else
         return run_transaction (db, "ROLLBACK TO SAVEPOINT", savepoint);
      end if;
   end transaction_rollback;


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
      Event.emit_debug (4, "executing pragma command '" & sql & "'");
      if not SQLite.prepare_sql (db, sql, stmt'Access) then
         if not silence then
            ERROR_SQLITE (db, func, sql);
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

end Core.CommonSQL;
