--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with sqlite_h;
private with Interfaces.C;
private with SQLite;

package Core.Repo.Operations.Schema is

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

   --  For empty database files, we import up to version 2013 immediately.
   --  Future versions are taken care of silently by the upgrade mechanism.
   function import_schema_2013 (db : sqlite_h.sqlite3_Access) return Action_Result;

   --  Checks user version against compiled version and upgrades as necessary
   --  RESULT_UPTODATE   - 1) repo version already matches compiled version (did nothing)
   --                      2) repo version was older, but successfully updated
   --  RESULT_FATAL      - 1) failed to read user version
   --                    - 2) failed to update to current version
   --                    - 3) database was opened as read-only when it needs updating
   --  RESULT_REPOSCHEMA - repo is too old or too new (major version mismatch) to continue
   --  RESULT_OK         - repo is newer but still compatible (do nothing)
   function repo_upgrade (db : sqlite_h.sqlite3_Access; reponame : String) return Action_Result;

   --  Initialize and finalize all prepared statements
   procedure repo_prstmt_finalize (db : in out sqlite_h.sqlite3_Access);
   function repo_prstmt_initialize (db : in out sqlite_h.sqlite3_Access) return Action_Result;

   --  Executes and resets REPO_VERSION prepared statement (single result)
   function retrieve_prepared_version (origin : Text) return String;

   --  executes DELETE prepared statement (no results)
   --  Return RESULT_OK upon success
   function kill_package (origin : Text) return Action_Result;

   function run_repo_prepared_statement
     (index : repository_stmt_index;
      args  : Set_Repo_Stmt_Args.Vector) return Boolean;

private

   package IC renames Interfaces.C;

   REPO_SCHEMA_MAJOR : constant Natural := 2;
   REPO_SCHEMA_MINOR : constant Natural := 14;
   REPO_SCHEMA_ALL   : constant Natural := REPO_SCHEMA_MAJOR * 1000 + REPO_SCHEMA_MINOR;

   --  REPO_SCHEMA_VERSION = REPO_SCHEMA_ALL
   REPO_SCHEMA_VERSION : constant String := "2014";

   type Upgrade_Series is range 2014 .. REPO_SCHEMA_ALL;

   type field is (SQL_string, summary);

   prepared_statements : array (repository_stmt_index) of SQLite.thick_stmt;

   --  Given a version that we want to upgrade to, return sql statements to get there
   function get_info (version : Upgrade_Series; info_type : field) return String;

   function repo_set_version
     (db      : sqlite_h.sqlite3_Access;
      nextver : Upgrade_Series) return Action_Result;

   function repo_apply_upgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      version  : Upgrade_Series) return Action_Result;

   --  SQL associated with prstmt_index enumeration
   function prstmt_text_sql (index : repository_stmt_index) return String;

end Core.Repo.Operations.Schema;
