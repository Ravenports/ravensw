--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with sqlite_h;

package SQLite is

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
end SQLite;
