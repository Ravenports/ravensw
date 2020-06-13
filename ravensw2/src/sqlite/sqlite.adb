--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Unchecked_Conversion;
with System;

package body SQLite is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   --------------------------------------------------------------------
   --  initialize_sqlite
   --------------------------------------------------------------------
   function initialize_sqlite return Boolean
   is
      result : IC.int;

      use type IC.int;
   begin
      result := sqlite_h.sqlite3_initialize;
      return (result = sqlite_h.SQLITE_OK);
   end initialize_sqlite;


   --------------------------------------------------------------------
   --  open_sqlite_database_readonly
   --------------------------------------------------------------------
   function open_sqlite_database_readonly (path : String;
                                           ppDB : not null access sqlite_h.sqlite3_Access)
                                           return Boolean
   is
      c_path  : ICS.chars_ptr;
      result  : IC.int;

      use type IC.int;
   begin
      c_path := ICS.New_String (path);
      result := sqlite_h.sqlite3_open_v2 (File_Name => c_path,
                                          Handle    => ppDB,
                                          flags     => sqlite_h.SQLITE_OPEN_READONLY,
                                          zVfs      => ICS.Null_Ptr);
      ICS.Free (c_path);
      return (result = sqlite_h.SQLITE_OK);
   end open_sqlite_database_readonly;


   --------------------------------------------------------------------
   --  open_sqlite_database_readwrite
   --------------------------------------------------------------------
   function open_sqlite_database_readwrite (path : String;
                                            ppDB : not null access sqlite_h.sqlite3_Access)
                                            return Boolean
   is
      c_path  : ICS.chars_ptr;
      result  : IC.int;

      use type IC.int;
   begin
      c_path := ICS.New_String (path);
      result := sqlite_h.sqlite3_open (File_Name => c_path,
                                       Handle    => ppDB);
      ICS.Free (c_path);
      return (result = sqlite_h.SQLITE_OK);
   end open_sqlite_database_readwrite;


   --------------------------------------------------------------------
   --  prepare_sql
   --------------------------------------------------------------------
   function prepare_sql
     (pDB    : sqlite_h.sqlite3_Access;
      sql    : String;
      stmt   : out thick_stmt) return Boolean
   is
      use type IC.int;

      c_sql     : ICS.chars_ptr;
      result    : IC.int;
      new_stmt  : thick_stmt;
      unlimited : constant IC.int := IC.int (-1);
      pzTail    : aliased ICS.chars_ptr := ICS.Null_Ptr;
   begin
      c_sql := ICS.New_String (sql);
      result := sqlite_h.sqlite3_prepare_v2 (db     => pDB,
                                             zSql   => c_sql,
                                             nByte  => unlimited,
                                             ppStmt => new_stmt.pStmt'Access,
                                             pzTail => pzTail'Access);
      ICS.Free (c_sql);
      stmt := new_stmt;
      stmt.initialized := True;
      return (result = sqlite_h.SQLITE_OK);
   end prepare_sql;


   --------------------------------------------------------------------
   --  step_to_another_row #1
   --------------------------------------------------------------------
   function step_to_another_row (stmt : thick_stmt) return Boolean
   is
      use type IC.int;

      result : IC.int;
   begin
      result := sqlite_h.sqlite3_step (stmt.pStmt);
      return (result = sqlite_h.SQLITE_ROW);
   end step_to_another_row;


   --------------------------------------------------------------------
   --  step_to_completion #1
   --------------------------------------------------------------------
   function step_to_completion (stmt : thick_stmt) return Boolean
   is
      use type IC.int;

      result : IC.int;
   begin
      result := sqlite_h.sqlite3_step (stmt.pStmt);
      return (result = sqlite_h.SQLITE_DONE);
   end step_to_completion;


   --------------------------------------------------------------------
   --  step_to_another_row #2
   --------------------------------------------------------------------
   function step_to_another_row
     (stmt        : thick_stmt;
      num_retries : Natural) return Boolean
   is
      use type IC.int;

      result : IC.int;
      slpres : IC.int;
      counter : Natural := 0;
   begin
      loop
         result := sqlite_h.sqlite3_step (stmt.pStmt);
         exit when Integer (result) /= sqlite_h.enum_error_types'Pos (sqlite_h.SQLITE_BUSY);
         exit when counter > num_retries;
         counter := counter + 1;
         slpres := sqlite_h.sqlite3_sleep (IC.int (200));
      end loop;
      return (result = sqlite_h.SQLITE_ROW);
   end step_to_another_row;


   --------------------------------------------------------------------
   --  step_to_completion #2
   --------------------------------------------------------------------
   function step_to_completion
     (stmt        : thick_stmt;
      num_retries : Natural) return Boolean
   is
      use type IC.int;

      result : IC.int;
      slpres : IC.int;
      counter : Natural := 0;
   begin
      loop
         result := sqlite_h.sqlite3_step (stmt.pStmt);
         exit when Integer (result) /= sqlite_h.enum_error_types'Pos (sqlite_h.SQLITE_BUSY);
         exit when counter > num_retries;
         counter := counter + 1;
         slpres := sqlite_h.sqlite3_sleep (IC.int (200));
      end loop;
      return (result = sqlite_h.SQLITE_DONE);
   end step_to_completion;


   --------------------------------------------------------------------
   --  step
   --------------------------------------------------------------------
   function step (stmt : thick_stmt) return Step_Result
   is
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_step (stmt.pStmt);
      case result is
         when sqlite_h.SQLITE_DONE =>
            return no_more_data;
         when sqlite_h.SQLITE_ROW =>
            return row_present;
         when others =>
            return something_else;
      end case;
   end step;


   --------------------------------------------------------------------
   --  retrieve_integer
   --------------------------------------------------------------------
   function retrieve_integer
     (stmt   : thick_stmt;
      column : Natural) return sql_int64
   is
      result : sqlite_h.sql64;
   begin
      result := sqlite_h.sqlite3_column_int64 (stmt.pStmt, IC.int (column));
      return sql_int64 (result);
   end retrieve_integer;


   --------------------------------------------------------------------
   --  retrieve_string
   --------------------------------------------------------------------
   function retrieve_string
     (stmt   : thick_stmt;
      column : Natural) return String
   is
      result : ICS.chars_ptr;
   begin
      --  Don't free result!
      result := sqlite_h.sqlite3_column_text (stmt.pStmt, IC.int (column));
      return ICS.Value (result);
   end retrieve_string;


   --------------------------------------------------------------------
   --  retrieve_boolean
   --------------------------------------------------------------------
   function retrieve_boolean
     (stmt   : thick_stmt;
      column : Natural) return Boolean
   is
      use type sqlite_h.sql64;

      result : sqlite_h.sql64;
   begin
      result := sqlite_h.sqlite3_column_int64 (stmt.pStmt, IC.int (column));
      return (result /= 0);
   end retrieve_boolean;


   --------------------------------------------------------------------
   --  finalize_statement
   --------------------------------------------------------------------
   procedure finalize_statement (stmt : in out thick_stmt)
   is
      procedure free_string (Position : Char_Pointer_Crate.Cursor);
      procedure really_free (Element : in out ICS.chars_ptr);

      procedure free_string (Position : Char_Pointer_Crate.Cursor) is
      begin
         stmt.char_pointers.Update_Element (Position, really_free'Access);
      end free_string;

      procedure really_free (Element : in out ICS.chars_ptr) is
      begin
         ICS.Free (Element);
      end really_free;

      result : IC.int;
   begin
      if stmt.initialized then
         result := sqlite_h.sqlite3_finalize (stmt.pStmt);
      end if;
      stmt.char_pointers.Iterate (free_string'Access);
      stmt.pStmt := null;
      stmt.initialized := False;
   end finalize_statement;


   --------------------------------------------------------------------
   --  close_database
   --------------------------------------------------------------------
   procedure close_database (db : sqlite_h.sqlite3_Access)
   is
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_close (db);
   end close_database;


   --------------------------------------------------------------------
   --  shutdown_sqlite
   --------------------------------------------------------------------
   procedure shutdown_sqlite
   is
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_shutdown;
   end shutdown_sqlite;


   --------------------------------------------------------------------
   --  sqlite3_get_auxdata_as_regex
   --------------------------------------------------------------------
   function sqlite3_get_auxdata_as_regex
     (context : sqlite_h.sqlite3_context_Access;
      N       : Integer) return regex_h.regex_t_Access
   is
      function convert is new Ada.Unchecked_Conversion (Source => sqlite_h.Void_Ptr,
                                                        Target => regex_h.regex_t_Access);
   begin
      return convert (sqlite_h.sqlite3_get_auxdata (context, IC.int (N)));
   end sqlite3_get_auxdata_as_regex;


   --------------------------------------------------------------------
   --  sqlite3_set_auxdata_as_regex
   --------------------------------------------------------------------
   procedure sqlite3_set_auxdata_as_regex
     (context  : sqlite_h.sqlite3_context_Access;
      N        : Integer;
      data     : regex_h.regex_t_Access;
      callback : cb_regex)
   is
      function convert2void is new Ada.Unchecked_Conversion (Source => regex_h.regex_t_Access,
                                                             Target => sqlite_h.Void_Ptr);
      function convert2callback is new Ada.Unchecked_Conversion (Source => cb_regex,
                                                                 Target => sqlite_h.cb_auxdata);

      data_ptr : sqlite_h.Void_Ptr;
      gen_cb   : sqlite_h.cb_auxdata;
   begin
      data_ptr := convert2void (data);
      gen_cb   := convert2callback (callback);

      sqlite_h.sqlite3_set_auxdata (context  => context,
                                    N        => IC.int (N),
                                    data     => data_ptr,
                                    callback => gen_cb);
   end sqlite3_set_auxdata_as_regex;


   --------------------------------------------------------------------
   --  db_connected
   --------------------------------------------------------------------
   function db_connected (db : sqlite_h.sqlite3_Access) return Boolean
   is
      use type sqlite_h.sqlite3_Access;
   begin
      return (db /= null);
   end db_connected;


   --------------------------------------------------------------------
   --  get_last_error_message
   --------------------------------------------------------------------
   function get_last_error_message (db : sqlite_h.sqlite3_Access) return String
   is
      c_msg : ICS.chars_ptr;
   begin
      c_msg := sqlite_h.sqlite3_errmsg (db);
      return ICS.Value (c_msg);
   end get_last_error_message;


   --------------------------------------------------------------------
   --  get_last_error_code
   --------------------------------------------------------------------
   function get_last_error_code (db : sqlite_h.sqlite3_Access) return sqlite_h.enum_error_types
   is
      code : IC.int;
   begin
      code := sqlite_h.sqlite3_errcode (db);

      return sqlite_h.enum_error_types'Val (Integer (code) - 1);
   end get_last_error_code;


   --------------------------------------------------------------------
   --  exec_sql
   --------------------------------------------------------------------
   function exec_sql (db : sqlite_h.sqlite3_Access; sql : String; msg : out Text) return Boolean
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      c_sql  : ICS.chars_ptr;
      res    : IC.int;
   begin
      msg := blank;
      c_sql := ICS.New_String (sql);
      res := sqlite_h.sqlite3_exec (db       => db,
                                    sql      => c_sql,
                                    callback => System.Null_Address,
                                    firstarg => System.Null_Address,
                                    errmsg   => errmsg'Address);
      ICS.Free (c_sql);
      if res = sqlite_h.SQLITE_OK then
         return True;
      else
         msg := SUS (ICS.Value (errmsg));
         ICS.Free (errmsg);
         return False;
      end if;
   end exec_sql;


   --------------------------------------------------------------------
   --  database_was_opened_readonly
   --------------------------------------------------------------------
   function database_was_opened_readonly
     (db     : sqlite_h.sqlite3_Access;
      dbname : String) return Boolean
   is
      use type IC.int;
      res      : IC.int;
      c_dbname : ICS.chars_ptr;
   begin
      c_dbname := ICS.New_String (dbname);
      res := sqlite_h.sqlite3_db_readonly (db, c_dbname);
      ICS.Free (c_dbname);
      return (res = IC.int (1));
   end database_was_opened_readonly;


   --------------------------------------------------------------------
   --  get_sql
   --------------------------------------------------------------------
   function get_sql (pStmt : sqlite_h.sqlite3_stmt_Access) return String
   is
      sql : ICS.chars_ptr;
   begin
      sql := sqlite_h.sqlite3_sql (pStmt);
      return ICS.Value (sql);
   end get_sql;


   --------------------------------------------------------------------
   --  set_sqlite_profile
   --------------------------------------------------------------------
   procedure set_sqlite_profile
     (db       : sqlite_h.sqlite3_Access;
      callback : sqlite_h.cb_trace)
   is
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_trace_v2 (db       => db,
                                        uMask    => sqlite_h.SQLITE_TRACE_PROFILE,
                                        callback => callback,
                                        pCtx     => System.Null_Address);
   end set_sqlite_profile;


   --------------------------------------------------------------------
   --  reset_statement
   --------------------------------------------------------------------
   function reset_statement
     (stmt : thick_stmt) return Boolean
   is
      use type IC.int;
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_reset (stmt.pStmt);
      return (res = sqlite_h.SQLITE_OK);
   end reset_statement;


   --------------------------------------------------------------------
   --  get_number_of_columns
   --------------------------------------------------------------------
   function get_number_of_columns
     (stmt : thick_stmt) return Integer
   is
      numcols : IC.int;
   begin
      numcols := sqlite_h.sqlite3_column_count (stmt.pStmt);
      return (Integer (numcols));
   end get_number_of_columns;


   --------------------------------------------------------------------
   --  get_column_name
   --------------------------------------------------------------------
   function get_column_name
     (stmt         : thick_stmt;
      column_index : Natural) return String
   is
      name : ICS.chars_ptr;
   begin
      --  Don't free result!
      name := sqlite_h.sqlite3_column_name (stmt.pStmt, IC.int (column_index));
      return ICS.Value (name);
   end get_column_name;


   --------------------------------------------------------------------
   --  bind_integer
   --------------------------------------------------------------------
   procedure bind_integer
     (stmt         : thick_stmt;
      column_index : Natural;
      value        : sql_int64)
   is
      c_value : constant sqlite_h.sql64 := sqlite_h.sql64 (value);
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_bind_int64 (Handle => stmt.pStmt,
                                          Index  => IC.int (column_index),
                                          Value  => c_value);
   end bind_integer;


   --------------------------------------------------------------------
   --  bind_string
   --------------------------------------------------------------------
   procedure bind_string
     (stmt         : in out thick_stmt;
      column_index : Natural;
      value        : String)
   is
      txt : ICS.chars_ptr;
      res : IC.int;
   begin
      txt := ICS.New_String (value);
      res := sqlite_h.sqlite3_bind_text (Handle     => stmt.pStmt,
                                         Index      => IC.int (column_index),
                                         Text       => txt,
                                         nBytes     => IC.int (-1),
                                         destructor => sqlite_h.SQLITE_STATIC);
      stmt.char_pointers.Append (txt);
   end bind_string;


   --------------------------------------------------------------------
   --  get_db_filename
   --------------------------------------------------------------------
   function get_db_filename (db : sqlite_h.sqlite3_Access; tag : String) return String
   is
      zdbname : ICS.chars_ptr;
      res     : ICS.chars_ptr;
   begin
      zdbname := ICS.New_String (tag);
      res := sqlite_h.sqlite3_db_filename (db, zdbname);
      ICS.Free (zdbname);

      --  don't free result!
      return ICS.Value (res);
   end get_db_filename;


   --------------------------------------------------------------------
   --  get_number_of_changes
   --------------------------------------------------------------------
   function get_number_of_changes (db : sqlite_h.sqlite3_Access) return Integer
   is
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_changes (db);
      return (Integer (res));
   end get_number_of_changes;


   --------------------------------------------------------------------
   --  database_corrupt
   --------------------------------------------------------------------
   function database_corrupt (db : sqlite_h.sqlite3_Access) return Boolean
   is
      use type sqlite_h.enum_error_types;
   begin
      return (SQLite.get_last_error_code (db) = sqlite_h.SQLITE_CORRUPT);
   end database_corrupt;

end SQLite;
