--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with sqlite_h;

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

   prepared_statements : array (prstmt_index) of aliased sqlite_h.sqlite3_stmt_Access;

   --  SQL associated with prstmt_index enumeration
   function prstmt_text_sql (index : prstmt_index) return String;

   --  Argument types associated with prstmt_index enumeration
   function prstmt_text_argtypes (index : prstmt_index) return String;

   --  Initialize and finalize all prepared statements
   procedure prstmt_finalize (db : in out RDB_Connection);
   function prstmt_initialize (db : in out RDB_Connection) return Action_Result;

   --  For empty database files, we import up to version 34 immediately.
   --  Future versions are taken care of silently by the upgrade mechanism.
   function import_schema_34 (db : sqlite_h.sqlite3_Access) return Action_Result;

   --  Check database for available upgrades, and implement if available..
   function rdb_upgrade (db : RDB_Connection) return Action_Result;


private

   type Upgrade_Series is range 35 .. DB_SCHEMA_ALL;

   function exec (db : sqlite_h.sqlite3_Access; sql : String) return Action_Result;

   --  Given a version that we want to upgrade to, return sql statements to get there
   function upgrade_sql_for_version (version : Upgrade_Series) return String;

end Core.Database.Operations.Schema;
