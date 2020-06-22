--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with sqlite_h;
with regex_h;
with Core.Strings;

private with Ada.Containers.Vectors;
private with Interfaces.C.Strings;

use Core;
use Core.Strings;

package SQLite is

   type sql_int64 is range -(2**63) .. +(2**63 - 1);
   type Step_Result is (row_present, no_more_data, something_else);
   type thick_stmt is private;

   primary_db_identity : constant String := "main";

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
     (pDB    : not null sqlite_h.sqlite3_Access;
      sql    : String;
      stmt   : out thick_stmt) return Boolean;

   --  Return True if row found after the step
   function step_to_another_row (stmt : thick_stmt) return Boolean;

   --  Return True if query done after the step (no data returned)
   function step_to_completion (stmt : thick_stmt) return Boolean;

   --  Step through prepared statement, returning indication of 3 possible results
   function step (stmt : thick_stmt) return Step_Result;

   --  Return True if row found after the step, attempt num_retries when SQLITE_BUSY encountered
   function step_to_another_row
     (stmt        : thick_stmt;
      num_retries : Natural) return Boolean;

   --  Return True if query done after the step, attempt num_retries when SQLITE_BUSY encountered
   function step_to_completion
     (stmt        : thick_stmt;
      num_retries : Natural) return Boolean;

   --  After stepping, return 64-bit integer from given column
   function retrieve_integer
     (stmt   : thick_stmt;
      column : Natural) return sql_int64;

   --  After stepping, return string from given column
   function retrieve_string
     (stmt   : thick_stmt;
      column : Natural) return String;

   --  After stepping, return boolean from given column
   function retrieve_boolean
     (stmt   : thick_stmt;
      column : Natural) return Boolean;

   --  Close statement after use, don't return result
   procedure finalize_statement (stmt : in out thick_stmt);

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

   function db_connected
     (db : sqlite_h.sqlite3_Access) return Boolean;

   function get_last_error_message
     (db : not null sqlite_h.sqlite3_Access) return String;

   function get_last_error_code
     (db : not null sqlite_h.sqlite3_Access) return sqlite_h.enum_error_types;

   function invalid_regex
     (db : not null sqlite_h.sqlite3_Access) return Boolean;

   procedure rdb_syscall_overload;
   pragma Import (C, rdb_syscall_overload);

   function exec_sql
     (db  : not null sqlite_h.sqlite3_Access;
      sql : String;
      msg : out Text) return Boolean;

   function database_was_opened_readonly
     (db     : not null sqlite_h.sqlite3_Access;
      dbname : String) return Boolean;

   function get_sql
     (pStmt : not null sqlite_h.sqlite3_stmt_Access) return String;

   procedure set_sqlite_profile
     (db       : not null sqlite_h.sqlite3_Access;
      callback : sqlite_h.cb_trace);

   function reset_statement
     (stmt : thick_stmt) return Boolean;

   function get_number_of_columns
     (stmt : thick_stmt) return Integer;

   function get_column_name
     (stmt         : thick_stmt;
      column_index : Natural) return String;

   procedure bind_integer
     (stmt         : thick_stmt;
      column_index : Natural;
      value        : sql_int64);

   procedure bind_string
     (stmt         : in out thick_stmt;
      column_index : Natural;
      value        : String);

   procedure clear_bindings
     (stmt         : in out thick_stmt);

   function get_db_filename
     (db  : not null sqlite_h.sqlite3_Access;
      tag : String) return String;

   function get_number_of_changes
     (db : not null sqlite_h.sqlite3_Access) return Integer;

   function database_corrupt
     (db : not null sqlite_h.sqlite3_Access) return Boolean;

private

   package Char_Pointer_Crate is new Ada.Containers.Vectors
     (Element_Type => Interfaces.C.Strings.chars_ptr,
      Index_Type   => Natural,
      "="          => Interfaces.C.Strings."=");

   type thick_stmt is
      record
         pStmt         : aliased sqlite_h.sqlite3_stmt_Access;
         char_pointers : Char_Pointer_Crate.Vector;
         initialized   : Boolean := False;
      end record;

   function translate_char_pointer (pointer : Interfaces.C.Strings.chars_ptr) return String;

end SQLite;
