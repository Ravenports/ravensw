--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Core.Repo.Operations is

   function open_repository (reponame : String; readonly : Boolean) return Action_Result;

   procedure close_all_open_repositories;

private

   --  The package repo schema version
   --  Minor schema changes don't prevent older ravensw versions accessing the repo.
   REPO_SCHEMA_MAJOR : constant Natural := 2;
   REPO_SCHEMA_MINOR : constant Natural := 14;
   REPO_SCHEMA_ALL   : constant Natural := REPO_SCHEMA_MAJOR * 1000 + REPO_SCHEMA_MINOR;

   --  REPO_SCHEMA_VERSION = REPO_SCHEMA_ALL
   REPO_SCHEMA_VERSION : constant String := "2014";

   type grade_range is range 2013 .. REPO_SCHEMA_ALL;

   type repository_stmt_index is
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

   prepared_statements : array (repository_stmt_index) of sqlite_h.sqlite3_stmt_Access;

   function run_transaction (db : sqlite_h.sqlite3_Access;
                             query : String;
                             savepoint : String) return Boolean;

   function trax_begin    (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean;
   function trax_commit   (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean;
   function trax_rollback (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean;

   function get_pragma (db      : sqlite_h.sqlite3_Access;
                        sql     : String;
                        res     : out int64;
                        silence : Boolean) return Action_Result;

   procedure close_repository (reponame : Text; commit : Boolean);

   function sqlite_filename (reponame : String) return String;
   function meta_filename (reponame : String) return String;

   function check_version (db : sqlite_h.sqlite3_Access; reponame : String) return Action_Result;

   function user_version (db : sqlite_h.sqlite3_Access; reposcver : out Integer) return Boolean;

   type grade_info is
      record
         identifier : grade_range;
         summary    : Text;
         query      : Text;
      end record;

   --  Given the current version, return the instructions to upgrade to next version
   function upgrade_info (current_version : grade_range) return grade_info;

   function repo_upgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      version  : Integer) return Action_Result;

   function repo_apply_upgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      version  : grade_range) return Action_Result;

   function repo_set_version
     (db : sqlite_h.sqlite3_Access;
      reposcver : grade_range) return Action_Result;

end Core.Repo.Operations;
