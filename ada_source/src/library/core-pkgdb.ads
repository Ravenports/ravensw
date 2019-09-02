--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;
with Core.Pkg;
with sqlite_h;
with SQLite;

private with regex_h;
private with System;
private with Core.Unix;

package Core.PkgDB is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   type T_match is (MATCH_ALL, MATCH_EXACT, MATCH_GLOB, MATCH_REGEX, MATCH_CONDITION);
   type T_pkgdb is (PKGDB_DEFAULT, PKGDB_REMOTE, PKGDB_MAYBE_REMOTE);

   type struct_pkgdb is limited private;
   type struct_pkgdb_Access is access all struct_pkgdb;

   procedure pkgshell_open (reponame : access ICS.chars_ptr);
   pragma Export (C, pkgshell_open);

   procedure pkgdb_command (passthrough : String);

   --  By default, MATCH_EXACT and MATCH_REGEX are case sensitive.  This
   --  is modified in many actions according to the value of
   --  CASE_SENSITIVE_MATCH in ravensw.conf and then possibly reset again in
   --  pkg search et al according to command line flags
   procedure pkgdb_set_case_sensitivity (sensitive : Boolean);

   --  The four arguments are mutually exclusive, but the command line parser doesn't
   --  object if more than one set.  Set in priority order exact, glob, regex, condition
   function set_match_behavior
     (request_exact : Boolean := False;
      request_glob  : Boolean := False;
      request_regex : Boolean := False;
      request_condition : Boolean := False) return T_match;

   --  Close access to the local sqlite database
   procedure pkgdb_close (db : in out struct_pkgdb);

   function pkgdb_transaction_begin    (db : struct_pkgdb; savepoint : String) return Boolean;
   function pkgdb_transaction_commit   (db : struct_pkgdb; savepoint : String) return Boolean;
   function pkgdb_transaction_rollback (db : struct_pkgdb; savepoint : String) return Boolean;
   function pkgdb_transaction_begin_sqlite    (db : sqlite_h.sqlite3_Access; savepoint : String)
                                               return Boolean;
   function pkgdb_transaction_commit_sqlite   (db : sqlite_h.sqlite3_Access; savepoint : String)
                                               return Boolean;
   function pkgdb_transaction_rollback_sqlite (db : sqlite_h.sqlite3_Access; savepoint : String)
                                               return Boolean;

   function pkgdb_open     (db : in out struct_pkgdb; dbtype : T_pkgdb)
                            return Core.Pkg.Pkg_Error_Type;
   function pkgdb_open_all (db : in out struct_pkgdb; dbtype : T_pkgdb; reponame : String)
                            return Core.Pkg.Pkg_Error_Type;

   function get_pragma (db      : sqlite_h.sqlite3_Access;
                        sql     : String;
                        res     : out SQLite.sql_int64;
                        silence : Boolean) return Core.Pkg.Pkg_Error_Type;

   procedure ERROR_SQLITE (db : sqlite_h.sqlite3_Access; func : String; query : String);

private

   case_sensitivity_setting : Boolean := False;

   type struct_pkgdb is limited
      record
         sqlite : aliased sqlite_h.sqlite3_Access;
         prstmt_initialized : Boolean;
         repos : Pkg.text_crate.Vector;
      end record;

   type sql_prstmt_index is
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

   --  SQL associated with sql_prstmt_index enumeration
   function prstmt_text_sql (index : sql_prstmt_index) return String;

   --  Argument types associated with sql_prstmt_index enumeration
   function prstmt_text_argtypes (index : sql_prstmt_index) return String;

   sql_prepared_statements : array (sql_prstmt_index) of aliased sqlite_h.sqlite3_stmt_Access;

   function  pkgdb_is_case_sensitive return Boolean;

   --  regex object must be global to assign access to it.
   re : aliased regex_h.regex_t;

   --  Defines custom sql functions for sqlite
   function pkgdb_sqlcmd_init
     (db       : not null sqlite_h.sqlite3_Access;
      pzErrMsg : access ICS.chars_ptr;
      pThunk   : sqlite_h.sqlite3_api_routines_Access) return IC.int;
   pragma Export (C, pkgdb_sqlcmd_init);

   --  select now();
   --  returns 1567046088
   --
   --  takes no arguments
   --  Function returns unix epoch as defined by system clock
   procedure pkgdb_now
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_now);

   --  select myarch();
   --  returns "DragonFly:5.8:x86_64"
   --  select myarch(null);
   --  returns "DragonFly:5.8:x86_64"
   --  select myarch("OpenBSD:6.4:amd64");
   --  returns "OpenBSD:6.4:amd64";
   --
   --  arg1 optional.  Will override ABI if present
   --  Function returns configured ABI unless overridden in arguments (then it returns that)
   procedure pkgdb_myarch
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_myarch);

   --  sqlite> select regexp ("[0-9]e", "abc3ef");
   --  returns 1
   --  sqlite> select regexp ("[0-9]f", "abc3ef");
   --  returns 0
   --
   --  arg1 = regular expression string
   --  arg2 = input string
   --  returns 0 (false) or 1 (true) if a match is found
   --  Function returns True if given string has a match against given regular expression
   procedure pkgdb_regex
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_regex);

   --  select split_version ("name", "joe-1.0");
   --  returns "joe"
   --  select split_version ("version", "joe-1.0_1,2");
   --  returns "1.0_1,2"
   --
   --  arg1 = "name" or "version"
   --  arg2 = package name
   --  function returns name part or version part of package name
   procedure pkgdb_split_version
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_split_version);

   --  select vercmp("<=", "joe-1.0", "joe-1.1");
   --  returns 1.
   --
   --  arg1 = operator string ("==", "!=", "<", ">", "<=", ">=", anything else)
   --  arg2 = package 1 name
   --  arg3 = package 2 name
   --  Function compares package 2 name against package 1 name and returns 0 or 1.
   procedure pkgdb_vercmp
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_vercmp);

   --  callback for pkgdb_regex
   procedure pkgdb_regex_delete (regex_ptr : not null regex_h.regex_t_Access);
   pragma Convention (C, pkgdb_regex_delete);

   --  Converts boolean until C integer
   function conv2cint (result : Boolean) return IC.int;

   --  Where split routine does the actual work (allows custom split words, delimiter, etc)
   procedure pkgdb_split_common
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access;
      delim   : Character;
      first   : String;
      second  : String);

   procedure prstmt_finalize (db : in out struct_pkgdb);
   function prstmt_initialize (db : in out struct_pkgdb) return Core.Pkg.Pkg_Error_Type;

   function run_transaction (db : sqlite_h.sqlite3_Access; query : String; savepoint : String)
                             return Boolean;

   function pkgdb_open_remote (db : in out struct_pkgdb; dbtype : T_pkgdb; reponame : String)
                               return Core.Pkg.Pkg_Error_Type;

   function vfs_dbdir_open (path : ICS.chars_ptr; flags : IC.int; mode : IC.int) return IC.int;
   pragma Export (C, vfs_dbdir_open);

   function vfs_dbdir_access (path : ICS.chars_ptr; mode : IC.int) return IC.int;
   pragma Export (C, vfs_dbdir_access);

   function vfs_dbdir_unlink (path : ICS.chars_ptr) return IC.int;
   pragma Export (C, vfs_dbdir_unlink);

   function vfs_dbdir_stat (path : ICS.chars_ptr; sb : Unix.struct_stat_Access) return IC.int;
   pragma Export (C, vfs_dbdir_stat);

   function vfs_dbdir_lstat (path : ICS.chars_ptr; sb : Unix.struct_stat_Access) return IC.int;
   pragma Export (C, vfs_dbdir_lstat);

   function vfs_dbdir_mkdir (path : ICS.chars_ptr; mode : IC.int) return IC.int;
   pragma Export (C, vfs_dbdir_mkdir);

   function pkgdb_init (db : sqlite_h.sqlite3_Access) return Core.Pkg.Pkg_Error_Type;

   function sql_exec (db : sqlite_h.sqlite3_Access; sql : String) return Core.Pkg.Pkg_Error_Type;

   function pkgdb_upgrade (db : struct_pkgdb) return Core.Pkg.Pkg_Error_Type;

   function upgrade_available (current_version : Natural) return Boolean;
   function upgrade_sql_for_next_version (current_version : Natural) return String;

   function pkgdb_open_repository (db       : in out struct_pkgdb;
                                   reponame : String) return Core.Pkg.Pkg_Error_Type;

   function pkgdb_profile_callback
     (trace_type : IC.unsigned;
      ud   : sqlite_h.Void_Ptr;
      stmt : sqlite_h.Void_Ptr;
      x    : sqlite_h.Void_Ptr) return IC.int;
   pragma Export (C, pkgdb_profile_callback);

end Core.PkgDB;
