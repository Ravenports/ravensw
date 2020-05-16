--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Finalization;

with Core.Pkgtypes;
with Core.Database;
with sqlite_h;

package Core.Repo.Iterator.Packages is

   illegal_match_style : exception;

   type SQLite_Iterator is limited private;
   type Match_Field is (none, origin, name, namever, comment, desc);

   procedure Reset (this : in out SQLite_Iterator);

   function count  (this : in out SQLite_Iterator) return Natural;

   function Next   (this       : in out SQLite_Iterator;
                    pkg_access : in out Pkgtypes.A_Package_Access;
                    behavior   : Iterator_Bahavior) return Action_Result;

   function initialize_as_provide
     (this     : in out SQLite_Iterator;
      reponame : String;
      pkgname  : String) return Action_Result;

   function initialize_as_shlib_provide
     (this     : in out SQLite_Iterator;
      reponame : String;
      pkgname  : String) return Action_Result;

   function initialize_as_require
     (this     : in out SQLite_Iterator;
      reponame : String;
      pkgname  : String) return Action_Result;

   function initialize_as_shlib_require
     (this     : in out SQLite_Iterator;
      reponame : String;
      pkgname  : String) return Action_Result;

   function initialize_as_search
     (this     : in out SQLite_Iterator;
      reponame : String;
      pattern  : String;
      field    : Match_Field;
      sortby   : Match_Field;
      match    : Database.Match_Behavior) return Action_Result;

   function initialize_as_standard_query
     (this     : in out SQLite_Iterator;
      reponame : String;
      pattern  : String;
      match    : Database.Match_Behavior) return Action_Result;

private

   type A_Variant is (standard_query, search, provide, require, shlib_provide, shlib_require);

   type SQLite_Iterator is
     new Ada.Finalization.Limited_Controlled with
      record
         stmt    : aliased sqlite_h.sqlite3_stmt_Access;
         counter : Natural;
         variant : A_Variant;
         mstyle  : Database.Match_Behavior;
         field   : Match_Field;
         fsort   : Match_Field;
         xrepo   : Text;
         pattern : Text;
         typeset : Boolean := False;
      end record;

   overriding procedure Finalize (this : in out SQLite_Iterator);

   function get_sql (this : SQLite_Iterator) return String;

   function search_how (this : SQLite_Iterator; field_name : String) return String;

   function search_condition (this : SQLite_Iterator) return String;

   function initialize_stmt (this : in out SQLite_Iterator) return Action_Result;

end Core.Repo.Iterator.Packages;
