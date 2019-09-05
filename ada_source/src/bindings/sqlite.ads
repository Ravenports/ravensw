--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with sqlite_h;
with regex_h;
with Core.Strings;

use Core;
use Core.Strings;

package SQLite is

   type sql_int64 is range -(2**63) .. +(2**63 - 1);

   --  return True on success
   function initialize_sqlite return Boolean;

   --  return True on success, db is output
   function open_sqlite_database_readonly
     (path  : String;
      ppDB  :  not null access sqlite_h.sqlite3_Access) return Boolean;

   --  return True on success, db is output
   function open_sqlite_database_readwrite
     (path  : String;
      ppDB  :  not null access sqlite_h.sqlite3_Access) return Boolean;

   --  return True on success, stmt is output
   function prepare_sql
     (pDB    : sqlite_h.sqlite3_Access;
      sql    : String;
      ppStmt : not null access sqlite_h.sqlite3_stmt_Access) return Boolean;

   --  return True if row found after the step
   function step_through_statement (stmt : sqlite_h.sqlite3_stmt_Access) return Boolean;

   --  return True if row found after the step, attempt num_retries when SQLITE_BUSY encountered
   function step_through_statement (stmt : sqlite_h.sqlite3_stmt_Access; num_retries : Natural)
                                    return Boolean;

   --  return True if row found after the step.  Set problem to True if result is not
   --  SQLITE_DONE or SQLITE_ROW
   function step_through_statement (stmt : sqlite_h.sqlite3_stmt_Access;
                                    problem : out Boolean) return Boolean;

   --  After stepping, return 64-bit integer from given column
   function retrieve_integer (stmt : sqlite_h.sqlite3_stmt_Access;
                              column : Natural) return sql_int64;

   --  After stepping, return string from given column
   function retrieve_string (stmt : sqlite_h.sqlite3_stmt_Access;
                             column : Natural) return String;

   --  After stepping, return boolean from given column
   function retrieve_boolean (stmt : sqlite_h.sqlite3_stmt_Access;
                             column : Natural) return Boolean;

   --  Close statement after use, don't return result
   procedure finalize_statement (stmt : sqlite_h.sqlite3_stmt_Access);

   --  Close an open database, don't check result
   procedure close_database (db : sqlite_h.sqlite3_Access);

   --  Shutdown sqlite3
   procedure shutdown_sqlite;

   function sqlite3_get_auxdata_as_regex
     (context : sqlite_h.sqlite3_context_Access;
      N       : Integer) return regex_h.regex_t_Access;

   type cb_regex is access procedure
     (regex_ptr :  not null regex_h.regex_t_Access);
   pragma Convention (C, cb_regex);

   procedure sqlite3_set_auxdata_as_regex
     (context  : sqlite_h.sqlite3_context_Access;
      N        : Integer;
      data     : regex_h.regex_t_Access;
      callback : cb_regex);

   function db_connected (db : sqlite_h.sqlite3_Access) return Boolean;

   function get_last_error_message (db : sqlite_h.sqlite3_Access) return String;

   function get_last_error_code (db : sqlite_h.sqlite3_Access) return sqlite_h.enum_error_types;

   procedure pkgdb_syscall_overload;
   pragma Import (C, pkgdb_syscall_overload);

   function exec_sql
     (db  : sqlite_h.sqlite3_Access;
      sql : String;
      msg : out Text) return Boolean;

   function database_was_opened_readonly
     (db     : sqlite_h.sqlite3_Access;
      dbname : String) return Boolean;

   function get_sql (pStmt : sqlite_h.sqlite3_stmt_Access) return String;

   procedure set_sqlite_profile
     (db       : sqlite_h.sqlite3_Access;
      callback : sqlite_h.cb_trace);

   function reset_statement (pStmt : sqlite_h.sqlite3_stmt_Access) return Boolean;

   function get_number_of_columns (pStmt : sqlite_h.sqlite3_stmt_Access) return Integer;

   function get_column_name (pStmt : sqlite_h.sqlite3_stmt_Access;
                             column_index : Natural) return String;

   procedure bind_integer
     (pStmt : sqlite_h.sqlite3_stmt_Access;
      column_index : Natural;
      value : sql_int64);

   procedure bind_string
     (pStmt : sqlite_h.sqlite3_stmt_Access;
      column_index : Natural;
      value : String);

end SQLite;
