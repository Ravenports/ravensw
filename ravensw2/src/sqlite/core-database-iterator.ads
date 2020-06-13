--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Finalization;

with Core.Pkgtypes;
private with SQLite;
private with sqlite_h;

package Core.Database.Iterator is

   db_illegal_match_style : exception;

   type DB_SQLite_Iterator is tagged limited private;
   type DB_Match_Field is (none, origin, name, namever, comment, desc);

   procedure Reset
     (this : in out DB_SQLite_Iterator);

   function count
     (this : in out DB_SQLite_Iterator) return Natural;

   function Next
     (this       : in out DB_SQLite_Iterator;
      pkg_access : Pkgtypes.A_Package_Access;
      sections   : Pkgtypes.Package_Load_Flags := (others => True))
      return Action_Result;

   function initialize_as_standard_query
     (this     : in out DB_SQLite_Iterator;
      conn     : Database.RDB_Connection;
      pattern  : String;
      match    : Database.Match_Behavior;
      just_one : Boolean) return Action_Result;

private

   internal_srcfile : constant String := "core-database-iterator.adb";

   type A_DB_Variant is (standard_query);
   type DB_Iterator_Bahavior is (once, cycled, auto);

   type DB_SQLite_Iterator is
     new Ada.Finalization.Limited_Controlled with
      record
         stmt    : SQLite.thick_stmt;
         dbconn  : sqlite_h.sqlite3_Access;
         counter : Natural;
         variant : A_DB_Variant;
         mstyle  : Database.Match_Behavior;
         cycles  : DB_Iterator_Bahavior;
         field   : DB_Match_Field;
         fsort   : DB_Match_Field;
         pattern : Text;
         typeset : Boolean := False;
         done    : Boolean := False;
         limit1  : Boolean := False;
      end record;

   overriding procedure Finalize (this : in out DB_SQLite_Iterator);

   function get_sql (this : DB_SQLite_Iterator) return String;

   function search_how (this : DB_SQLite_Iterator; field_name : String) return String;

   function search_condition (this : DB_SQLite_Iterator) return String;

   function initialize_stmt (this : in out DB_SQLite_Iterator) return Action_Result;

   NOT_INITIALIZED : constant String := "Iterator has not been initialized";

end Core.Database.Iterator;
