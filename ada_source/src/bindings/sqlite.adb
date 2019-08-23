--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C;
with Interfaces.C.Strings;

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
   --  open_sqlite_database
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
   --  prepare_sql
   --------------------------------------------------------------------
   function prepare_sql
     (pDB    : sqlite_h.sqlite3_Access;
      sql    : String;
      ppStmt : not null access sqlite_h.sqlite3_stmt_Access) return Boolean
   is
      use type IC.int;

      c_sql     : ICS.chars_ptr;
      result    : IC.int;
      unlimited : constant IC.int := IC.int (-1);
      pzTail    : aliased ICS.chars_ptr := ICS.Null_Ptr;
   begin
      c_sql := ICS.New_String (sql);
      result := sqlite_h.sqlite3_prepare_v2 (db     => pDB,
                                             zSql   => c_sql,
                                             nByte  => unlimited,
                                             ppStmt => ppStmt,
                                             pzTail => pzTail'Access);
      ICS.Free (c_sql);
      return (result = sqlite_h.SQLITE_OK);
   end prepare_sql;

end SQLite;
