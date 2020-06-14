--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Finalization;

with Core.Pkgtypes;
with Core.Database;
with SQLite;

package Core.Repo.Iterator.Packages is

   illegal_match_style : exception;

   type SQLite_Iterator is tagged limited private;
   type SQlite_Iterator_Access is access all SQLite_Iterator;

   type Match_Field is (none, origin, name, namever, comment, desc);

   procedure Reset (this : in out SQLite_Iterator);

   function count  (this : in out SQLite_Iterator) return Natural;

   function Next
     (this       : in out SQLite_Iterator;
      pkg_access : Pkgtypes.A_Package_Access;
      sections   : Pkgtypes.Package_Load_Flags := (others => True))
      return Action_Result;

   procedure rebind
     (this       : in out SQLite_Iterator;
      pattern    : String);

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
      match    : Database.Match_Behavior;
      just_one : Boolean) return Action_Result;

   function initialize_as_standard_query
     (this     : in out SQLite_Iterator;
      reponame : String;
      pattern  : String;
      match    : Database.Match_Behavior;
      just_one : Boolean) return Action_Result;

private

   internal_srcfile : constant String := "core-repo-iterator-packages.adb";

   type A_Variant is (standard_query, search, provide, require, shlib_provide, shlib_require);

   type SQLite_Iterator is
     new Ada.Finalization.Limited_Controlled with
      record
         stmt    : SQLite.thick_stmt;
         counter : Natural;
         variant : A_Variant;
         mstyle  : Database.Match_Behavior;
         cycles  : Iterator_Bahavior;
         field   : Match_Field;
         fsort   : Match_Field;
         xrepo   : Text;
         pattern : Text;
         typeset : Boolean := False;
         done    : Boolean := False;
         limit1  : Boolean := False;
      end record;

   overriding procedure Finalize (this : in out SQLite_Iterator);

   function get_sql (this : SQLite_Iterator) return String;

   function search_how (this : SQLite_Iterator; field_name : String) return String;

   function search_condition (this : SQLite_Iterator) return String;

   function initialize_stmt (this : in out SQLite_Iterator) return Action_Result;

   NOT_INITIALIZED : constant String := "Iterator has not been initialized";

end Core.Repo.Iterator.Packages;
