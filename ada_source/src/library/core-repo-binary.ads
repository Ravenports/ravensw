--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with sqlite_h;
with Core.PkgDB;
with Core.Iterators.Binary_sqlite;

use Core.Iterators;

package Core.Repo.Binary is

   type Repo_Operations_Binary is new Base_Repo_Operations with private;

   overriding
   function repo_init   (this : Repo_Operations_Binary; reponame : Text) return Boolean;

   overriding
   function repo_create (this : Repo_Operations_Binary; reponame : Text) return Boolean;

   overriding
   function repo_update (this : Repo_Operations_Binary; reponame : Text; force : Boolean)
                         return Pkg_Error_Type;

   overriding
   function repo_close  (this : Repo_Operations_Binary; reponame : Text; commit : Boolean)
                         return Boolean;

   overriding
   function repo_open   (this : Repo_Operations_Binary; reponame : Text; mode : mode_t)
                         return Boolean;

   overriding
   function repo_access (this : Repo_Operations_Binary; reponame : Text; mode : mode_t)
                         return Pkg_Error_Type;

   overriding
   function repo_ensure_loaded
     (this     : Repo_Operations_Binary;
      reponame : Text;
      pkg_ptr  : in out T_pkg_Access;
      flags    : Load_Flags)
      return Boolean;

   function get_cached_name
     (this     : Repo_Operations_Binary;
      reponame : Text;
      pkg_ptr  : in out T_pkg_Access)
      return String;

private
   --  The package repo schema major revision */
   REPO_SCHEMA_MAJOR : constant Natural := 2;

   --  The package repo schema minor revision.
   --  Minor schema changes don't prevent older ravensw versions accessing the repo.
   REPO_SCHEMA_MINOR : constant Natural := 14;

   --  REPO_SCHEMA_VERSION = REPO_SCHEMA_MAJOR * 1000 + REPO_SCHEMA_MINOR
   REPO_SCHEMA_VERSION : constant String := "2014";

   type Repo_Operations_Binary is new Base_Repo_Operations with
      record
         variant : repo_ops_variant := Pkg.binary;
      end record;

   type binary_stmt_index is
     (PKG,
      DEPS,
      CAT1,
      CAT2,
      LIC1,
      LIC2,
      OPT1,
      OPT2,
      SHLIB1,
      SHLIB_REQD,
      SHLIB_PROV,
      ANNOTATE1,
      ANNOTATE2,
      EXISTS,
      REPO_VERSION,
      DELETE,
      PROVIDE,
      PROVIDES,
      REQUIRE,
      REQUIRES
     );

   type upgrade_range   is range 2013 .. 2000 + REPO_SCHEMA_MINOR - 1;
   type downgrade_range is range 2013 .. 2000 + REPO_SCHEMA_MINOR - 1;

      --  SQL associated with sql_prstmt_index enumeration
   function binary_stmt_text_sql (index : binary_stmt_index) return String;

   --  Argument types associated with sql_prstmt_index enumeration
   function binary_stmt_text_argtypes (index : binary_stmt_index) return String;

   binary_prepared_statements : array (binary_stmt_index) of sqlite_h.sqlite3_stmt_Access;

   function pkg_repo_binary_get_user_version
     (db : sqlite_h.sqlite3_Access;
      reposcver : out Integer) return Boolean;

   function pkg_repo_binary_set_version
     (db : sqlite_h.sqlite3_Access;
      reposcver : Integer) return Pkg_Error_Type;

   function pkg_repo_binary_check_version
     (db   : sqlite_h.sqlite3_Access; reponame : String) return Pkg_Error_Type;

   function pkg_repo_binary_upgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      current_version : Integer) return Pkg_Error_Type;

   function pkg_repo_binary_downgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      current_version : Integer) return Pkg_Error_Type;

   function next_version (given_version : upgrade_range) return Positive;
   function upgrade_message (given_version : upgrade_range) return String;
   function upgrade_sql (given_version : upgrade_range) return String;

   function previous_version (given_version : downgrade_range) return Positive;
   function downgrade_message (given_version : downgrade_range) return String;
   function downgrade_sql (given_version : downgrade_range) return String;

   function pkg_repo_binary_apply_change
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      current_version : Integer;
      upgrade : Boolean) return Pkg_Error_Type;

   function pkg_repo_binary_query
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      pattern  : String;
      match    : PkgDB.T_match;
      flags    : Iterator_Flags) return Binary_sqlite.Iterator_Binary_Sqlite;

end Core.Repo.Binary;
