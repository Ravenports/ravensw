--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with sqlite_h;

package SQLite is

   type sql_int64 is range -(2**63) .. +(2**63 - 1);

   --  return True on success
   function initialize_sqlite return Boolean;

   --  return True on success, db is output
   function open_sqlite_database_readonly
     (path  : String;
      ppDB  :  not null access sqlite_h.sqlite3_Access) return Boolean;

   --  return True on success, stmt is output
   function prepare_sql
     (pDB    : sqlite_h.sqlite3_Access;
      sql    : String;
      ppStmt : not null access sqlite_h.sqlite3_stmt_Access) return Boolean;

   --  return True if row found after the step
   function step_through_statement (stmt : sqlite_h.sqlite3_stmt_Access) return Boolean;

   --  After stepping, return 64-bit integer from given column
   function retrieve_integer (stmt : sqlite_h.sqlite3_stmt_Access;
                              column : Integer) return sql_int64;

   --  Close statement after use, don't return result
   procedure finalize_statement (stmt : sqlite_h.sqlite3_stmt_Access);

   --  Close an open database, don't check result
   procedure close_database (db : sqlite_h.sqlite3_Access);

   --  Shutdown sqlite3
   procedure shutdown_sqlite;

end SQLite;
