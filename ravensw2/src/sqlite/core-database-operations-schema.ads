--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with sqlite_h;
private with SQLite;

package Core.Database.Operations.Schema is

   type prstmt_index is
     (MTREE,
      PKG,
      DEPS_UPDATE,
      DEPENDENCIES,
      FILES,
      FILES_REPLACE,
      DIRS1,
      DIRS2,
      CATEGORY1,
      CATEGORY2,
      LICENSES1,
      LICENSES2,
      USERS1,
      USERS2,
      GROUPS1,
      GROUPS2,
      SCRIPT1,
      SCRIPT2,
      OPTION1,
      OPTION2,
      SHLIBS1,
      SHLIBS_REQD,
      SHLIBS_PROV,
      ANNOTATE1,
      ANNOTATE2,
      ANNOTATE_ADD1,
      ANNOTATE_DEL1,
      ANNOTATE_DEL2,
      CONFLICT,
      PKG_PROVIDE,
      PROVIDE,
      UPDATE_DIGEST,
      CONFIG_FILES,
      UPDATE_CONFIG_FILE,
      PKG_REQUIRE,
      REQUIRE
     );

   --  Initialize and finalize all prepared statements
   procedure local_prstmt_finalize (db : in out RDB_Connection);
   function local_prstmt_initialize (db : in out RDB_Connection) return Action_Result;

   --  For empty database files, we import up to version 34 immediately.
   --  Future versions are taken care of silently by the upgrade mechanism.
   function import_schema_34 (db : sqlite_h.sqlite3_Access) return Action_Result;

   --  Check database for available upgrades, and implement if available..
   function rdb_upgrade (db : RDB_Connection) return Action_Result;

   --  Given already-prepared statement index and its arguments, execute the query
   function run_prepared_statement
     (index : prstmt_index;
      args  : Set_Stmt_Args.Vector) return Boolean;

private

   internal_srcfile : constant String := "core-database-operations-schema.adb";

   type Upgrade_Series is range 35 .. DB_SCHEMA_ALL;

   prepared_statements : array (prstmt_index) of SQLite.thick_stmt;

   --  Given a version that we want to upgrade to, return sql statements to get there
   function upgrade_sql_for_version (version : Upgrade_Series) return String;

   --  SQL associated with prstmt_index enumeration
   function prstmt_text_sql (index : prstmt_index) return String;


end Core.Database.Operations.Schema;
