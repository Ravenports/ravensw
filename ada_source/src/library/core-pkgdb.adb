--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Conversions;
with Ada.Characters.Latin_1;
with Ada.Directories;
with System;

with Core.Config;
with Core.Deps;
with Core.Version;
with Core.Repo_Operations;
with Core.Event;
with Core.Pkg;     use Core.Pkg;
with Core.Strings; use Core.Strings;
with SQLite;

package body Core.PkgDB is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;

   --------------------------------------------------------------------
   --  pkgshell_open
   --------------------------------------------------------------------
   procedure pkgshell_open (reponame : access ICS.chars_ptr)
   is
      dbdir  : constant String := Config.pkg_config_get_string (Config.conf_dbdir);
      dbfile : constant String := dbdir & "/local.sqlite";
      result : IC.int;
   begin
      reponame.all := ICS.New_String (dbfile);
      result := sqlite_h.sqlite3_auto_extension (callback => pkgdb_sqlcmd_init'Access);
   end pkgshell_open;


   --------------------------------------------------------------------
   --  pkgdb_command
   --------------------------------------------------------------------
   procedure pkgdb_command  (passthrough : String)
   is
      numargs : Natural := count_char (passthrough, LAT.Vertical_Line) + 1;
   begin
      if not SQLite.initialize_sqlite then
         return;
      end if;

      declare
         type argv_t is array (1 .. numargs) of aliased ICS.chars_ptr;
         delim : constant String (1 .. 1) := (1 => LAT.Vertical_Line);
         argv    : argv_t;
         argsval : access ICS.chars_ptr;
         result  : IC.int;
      begin
         for x in 1 .. numargs loop
            argv (x) := ICS.New_String (specific_field (passthrough, x, delim));
         end loop;

         argsval := argv (1)'Access;

         result := sqlite_h.sqlite3_shell (IC.int (numargs), argsval);

         for x in 1 .. numargs loop
            ICS.Free (argv (x));
         end loop;
      end;
   end pkgdb_command;


   --------------------------------------------------------------------
   --  pkgdb_sqlcmd_init
   --------------------------------------------------------------------
   function pkgdb_sqlcmd_init
     (db       : not null sqlite_h.sqlite3_Access;
      pzErrMsg : not null access ICS.chars_ptr;
      pThunk   : not null sqlite_h.sqlite3_api_routines_Access) return IC.int
   is
      procedure fast (name : String; nargs : Natural; cb : sqlite_h.cb_xFuncStep);
      procedure fast (name : String; nargs : Natural; cb : sqlite_h.cb_xFuncStep)
      is
         use type IC.int;
         flags : constant IC.int := sqlite_h.SQLITE_ANY + sqlite_h.SQLITE_DETERMINISTIC;
         result : IC.int;
      begin
         result := sqlite_h.sqlite3_create_function
           (db            => db,
            zFunctionName => ICS.New_String (name),
            nArg          => IC.int (nargs),
            eTextRep      => flags,
            pApp          => System.Null_Address,
            xFunc         => cb,
            xStep         => null,
            xFinal        => null);
      end fast;
   begin
      fast ("now",    0, pkgdb_now'Access);
      fast ("myarch", 0, pkgdb_myarch'Access);
      fast ("myarch", 1, pkgdb_myarch'Access);
      fast ("regexp", 2, pkgdb_regex'Access);
      fast ("vercmp", 3, pkgdb_vercmp'Access);
      fast ("split_version", 2, pkgdb_split_version'Access);

      return sqlite_h.SQLITE_OK;
   end pkgdb_sqlcmd_init;


   --------------------------------------------------------------------
   --  pkgdb_vercmp
   --------------------------------------------------------------------
   procedure pkgdb_vercmp
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      argv   : array (1 .. 3) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if numargs /= 3 then
         errmsg := ICS.New_String ("Invalid usage of vercmp(): needs 3 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      declare
         op_str : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         arg1   : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (2));
         arg2   : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (3));
         op     : Deps.pkg_dep_version_op;
         cmp    : Version.cmp_result;
         ret    : Boolean;
         result : IC.int := 0;
         use type ICS.chars_ptr;
      begin
         if op_str = ICS.Null_Ptr or else
           arg1 = ICS.Null_Ptr or else
           arg2 = ICS.Null_Ptr
         then
            errmsg := ICS.New_String ("Invalid usage of vercmp(): null arguments");
            sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
            ICS.Free (errmsg);
            return;
         end if;

         op := Deps.pkg_deps_string_toop (ICS.Value (op_str));
         cmp := Version.pkg_version_cmp (ICS.Value (arg1), ICS.Value (arg2));

         case op is
            when Deps.VERSION_EQ  => ret := (cmp = 0);
            when Deps.VERSION_GE  => ret := (cmp >= 0);
            when Deps.VERSION_LE  => ret := (cmp <= 0);
            when Deps.VERSION_GT  => ret := (cmp > 0);
            when Deps.VERSION_LT  => ret := (cmp < 0);
            when Deps.VERSION_NOT => ret := (cmp /= 0);
            when Deps.VERSION_ANY => ret := True;
         end case;

         if ret then
            result := IC.int (1);
         end if;
         sqlite_h.sqlite3_result_int (context, result);
      end;
   end pkgdb_vercmp;


   --------------------------------------------------------------------
   --  pkgdb_regex
   --------------------------------------------------------------------
   procedure pkgdb_regex
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      argv   : array (1 .. 2) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if numargs /= 2 then
         errmsg := ICS.New_String ("Invalid usage of regex(): needs 2 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      declare
         regex     : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         str       : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (2));
         re_Access : regex_h.regex_t_Access;
         ret       : IC.int;

         use type ICS.chars_ptr;
         use type regex_h.regex_t_Access;
      begin
         if regex = ICS.Null_Ptr or else
           str = ICS.Null_Ptr
         then
            errmsg := ICS.New_String ("Invalid usage of regex(): null arguments");
            sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
            ICS.Free (errmsg);
            return;
         end if;

         re_Access := SQLite.sqlite3_get_auxdata_as_regex (context, 0);

         if re_Access = null then
            declare
               cflags : IC.int;
               res    : IC.int;
            begin
               if pkgdb_is_case_sensitive then
                  cflags := IC.int (regex_h.REG_EXTENDED + regex_h.REG_NOSUB);
               else
                  cflags := IC.int (regex_h.REG_EXTENDED + regex_h.REG_NOSUB + regex_h.REG_ICASE);
               end if;

               re_Access := re'Access;
               res := regex_h.regcomp (re_Access, regex, cflags);
               if (res /= 0) then
                  errmsg := ICS.New_String ("Invalid regex");
                  sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
                  ICS.Free (errmsg);
                  return;
               end if;

               SQLite.sqlite3_set_auxdata_as_regex (context  => context,
                                                    N        => 0,
                                                    data     => re_Access,
                                                    callback => pkgdb_regex_delete'Access);
            end;
         end if;
         ret := regex_h.regexec (preg   => re_Access,
                                 regex  => str,
                                 nmatch => 0,
                                 pmatch => null,
                                 eflags => 0);
         sqlite_h.sqlite3_result_int (context => context,
                                      result  => conv2cint (ret /= regex_h.REG_NOMATCH));
      end;
   end pkgdb_regex;


   --------------------------------------------------------------------
   --  pkgdb_myarch
   --------------------------------------------------------------------
   procedure pkgdb_myarch
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type ICS.chars_ptr;
      use type IC.int;

      errmsg   : ICS.chars_ptr;
      first    : ICS.chars_ptr := ICS.Null_Ptr;
      show_abi : Boolean := False;
   begin
      if numargs > 1 then
         errmsg := ICS.New_String ("Invalid usage of myarch(): needs 0 or 1 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      if numargs = 0 then
         show_abi := True;
      else
         first := sqlite_h.sqlite3_value_text (argsval.all);
         if first = ICS.Null_Ptr then
            show_abi := True;
         end if;
      end if;

      if show_abi then
         declare
            abi : constant String := Config.pkg_config_get_string (Config.conf_abi);
         begin
            first := ICS.New_String (abi);
            sqlite_h.sqlite3_result_text (context    => context,
                                          result     => first,
                                          termpos    => -1,
                                          destructor => null);
            ICS.Free (first);
         end;
      else
         sqlite_h.sqlite3_result_text (context    => context,
                                       result     => first,
                                       termpos    => -1,
                                       destructor => null);
      end if;

   end pkgdb_myarch;


   --------------------------------------------------------------------
   --  pkgdb_split_version
   --------------------------------------------------------------------
   procedure pkgdb_split_version
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access) is
   begin
      pkgdb_split_common (context => context,
                          numargs => numargs,
                          argsval => argsval,
                          delim   => '-',
                          first   => "name",
                          second  => "version");
   end pkgdb_split_version;


   --------------------------------------------------------------------
   --  pkgdb_now
   --------------------------------------------------------------------
   procedure pkgdb_now
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      Now    : Ada.Calendar.Time := Ada.Calendar.Clock;
      epoch  : IC.long := Ada.Calendar.Conversions.To_Unix_Time (Now);
   begin
      if numargs /= 0 then
         errmsg := ICS.New_String ("Invalid usage of now(): no arguments expected");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      sqlite_h.sqlite3_result_int64 (context, sqlite_h.sql64 (epoch));
   end pkgdb_now;


   --------------------------------------------------------------------
   --  pkgdb_set_case_sensitivity
   --------------------------------------------------------------------
   procedure pkgdb_set_case_sensitivity (sensitive : Boolean) is
   begin
      case_sensitivity_setting := sensitive;
   end pkgdb_set_case_sensitivity;


   --------------------------------------------------------------------
   --  pkgdb_get_case_sensitivity
   --------------------------------------------------------------------
   function pkgdb_is_case_sensitive return Boolean is
   begin
      return case_sensitivity_setting;
   end pkgdb_is_case_sensitive;


   --------------------------------------------------------------------
   --  pkgdb_regex_delete
   --------------------------------------------------------------------
   procedure pkgdb_regex_delete (regex_ptr : not null regex_h.regex_t_Access) is
   begin
      regex_h.regfree (regex_ptr);
   end pkgdb_regex_delete;


   --------------------------------------------------------------------
   --  conv2cint
   --------------------------------------------------------------------
   function conv2cint (result : Boolean) return IC.int is
   begin
      if result then
         return IC.int (1);
      else
         return IC.int (0);
      end if;
   end conv2cint;


   --------------------------------------------------------------------
   --  pkgdb_split_common
   --------------------------------------------------------------------
   procedure pkgdb_split_common
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access;
      delim   : Character;
      first   : String;
      second  : String)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      argv   : array (1 .. 2) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if numargs /= 2 then
         errmsg := ICS.New_String ("Invalid usage of split_*(): needs 2 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      declare
         what : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         data : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (2));

         use type ICS.chars_ptr;
      begin
         if what = ICS.Null_Ptr or else
           data = ICS.Null_Ptr
         then
            errmsg := ICS.New_String ("Invalid usage of split_*(): null arguments");
            sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
            ICS.Free (errmsg);
            return;
         end if;

         declare
            whatstr  : constant String := ICS.Value (what);
            datastr  : constant String := ICS.Value (data);
            delimstr : constant String (1 .. 1) := (1 => delim);
         begin
            if whatstr = first then
               if contains (datastr, delimstr) then
                  sqlite_h.sqlite3_result_text
                    (context    => context,
                     result     => data,
                     termpos    => IC.int (head (datastr, delimstr)'Length),
                     destructor => null);
               else
                  sqlite_h.sqlite3_result_text (context    => context,
                                                result     => data,
                                                termpos    => IC.int (-1),
                                                destructor => null);
               end if;
            elsif whatstr = second then
               if contains (datastr, delimstr) then
                  declare
                     tailstring : constant String := tail (datastr, delimstr);
                     newstr     : ICS.chars_ptr;
                  begin
                     newstr := ICS.New_String (tailstring);
                     sqlite_h.sqlite3_result_text (context    => context,
                                                   result     => newstr,
                                                   termpos    => IC.int (-1),
                                                   destructor => null);
                     ICS.Free (newstr);
                  end;
               else
                  sqlite_h.sqlite3_result_text (context    => context,
                                                result     => data,
                                                termpos    => IC.int (-1),
                                                destructor => null);
               end if;
            else
               errmsg := ICS.New_String ("SQL function split_*() called with invalid arguments");
               sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
               ICS.Free (errmsg);
               return;
            end if;
         end;
      end;
   end pkgdb_split_common;


   --------------------------------------------------------------------
   --  set_match_behavior
   --------------------------------------------------------------------
   function set_match_behavior
     (request_exact : Boolean := False;
      request_glob  : Boolean := False;
      request_regex : Boolean := False;
      request_condition : Boolean := False) return T_match
   is
   begin
      if request_exact then
         return MATCH_EXACT;
      elsif request_glob then
         return MATCH_GLOB;
      elsif request_regex then
         return MATCH_REGEX;
      elsif request_condition then
         return MATCH_CONDITION;
      else
         return MATCH_ALL;
      end if;
   end set_match_behavior;


   --------------------------------------------------------------------
   --  prstmt_text_argtypes
   --------------------------------------------------------------------
   function prstmt_text_argtypes (index : sql_prstmt_index) return String is
   begin
      case index is
         when MTREE              => return "T";
         when PKG                => return "TTTTTTTTTTIIITTTI";
         when DEPS_UPDATE        => return "TTT";
         when DEPENDENCIES       => return "TTTI";
         when FILES              => return "TTI";
         when FILES_REPLACE      => return "TTI";
         when DIRS1              => return "T";
         when DIRS2              => return "ITI";
         when CATEGORY1          => return "T";
         when CATEGORY2          => return "IT";
         when LICENSES1          => return "T";
         when LICENSES2          => return "IT";
         when USERS1             => return "T";
         when USERS2             => return "IT";
         when GROUPS1            => return "T";
         when GROUPS2            => return "IT";
         when SCRIPT1            => return "T";
         when SCRIPT2            => return "TII";
         when OPTION1            => return "T";
         when OPTION2            => return "ITT";
         when SHLIBS1            => return "T";
         when SHLIBS_REQD        => return "IT";
         when SHLIBS_PROV        => return "IT";
         when ANNOTATE1          => return "T";
         when ANNOTATE2          => return "ITT";
         when ANNOTATE_ADD1      => return "TTTT";
         when ANNOTATE_DEL1      => return "TTT";
         when ANNOTATE_DEL2      => return "";
         when CONFLICT           => return "IT";
         when PKG_PROVIDE        => return "IT";
         when PROVIDE            => return "T";
         when UPDATE_DIGEST      => return "TI";
         when CONFIG_FILES       => return "TTI";
         when UPDATE_CONFIG_FILE => return "TT";
         when PKG_REQUIRE        => return "IT";
         when REQUIRE            => return "T";
      end case;
   end prstmt_text_argtypes;


   --------------------------------------------------------------------
   --  prstmt_text_sql
   --------------------------------------------------------------------
   function prstmt_text_sql (index : sql_prstmt_index) return String is
   begin
      case index is
         when MTREE =>
            return "INSERT OR IGNORE INTO mtree(content) VALUES(?1)";
         when PKG =>
            return "INSERT OR REPLACE INTO packages (origin, name, version, comment, desc, " &
              "message, arch, maintainer, www, prefix, flatsize, automatic, licenselogic, " &
              "mtree_id, time, manifestdigest, dep_formula, vital) " &
              "VALUES( ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, " &
              "(SELECT id FROM mtree WHERE content = ?14), NOW(), ?15, ?16, ?17 )";
         when DEPS_UPDATE =>
            return "UPDATE deps SET origin=?1, version=?2 WHERE name=?3";
         when DEPENDENCIES =>
            return "INSERT INTO deps (origin, name, version, package_id) VALUES (?1, ?2, ?3, ?4)";
         when FILES =>
            return "INSERT INTO files (path, sha256, package_id) VALUES (?1, ?2, ?3)";
         when FILES_REPLACE =>
            return "INSERT OR REPLACE INTO files (path, sha256, package_id) VALUES (?1, ?2, ?3)";
         when DIRS1 =>
            return "INSERT OR IGNORE INTO directories(path) VALUES(?1)";
         when DIRS2 =>
            return "INSERT INTO pkg_directories(package_id, directory_id, try) " &
              "VALUES (?1, (SELECT id FROM directories WHERE path = ?2), ?3)";
         when CATEGORY1 =>
            return "INSERT OR IGNORE INTO categories(name) VALUES(?1)";
         when CATEGORY2 =>
            return "INSERT INTO pkg_categories(package_id, category_id) " &
              "VALUES (?1, (SELECT id FROM categories WHERE name = ?2))";
         when LICENSES1 =>
            return "INSERT OR IGNORE INTO licenses(name) VALUES(?1)";
         when LICENSES2 =>
            return "INSERT INTO pkg_licenses(package_id, license_id) " &
              "VALUES (?1, (SELECT id FROM licenses WHERE name = ?2))";
         when USERS1 =>
            return "INSERT OR IGNORE INTO users(name) VALUES(?1)";
         when USERS2 =>
            return "INSERT INTO pkg_users(package_id, user_id) " &
              "VALUES (?1, (SELECT id FROM users WHERE name = ?2))";
         when GROUPS1 =>
            return "INSERT OR IGNORE INTO groups(name) VALUES(?1)";
         when GROUPS2 =>
            return "INSERT INTO pkg_groups(package_id, group_id) " &
              "VALUES (?1, (SELECT id FROM groups WHERE name = ?2))";
         when SCRIPT1 =>
            return "INSERT OR IGNORE INTO script(script) VALUES (?1)";
         when SCRIPT2 =>
            return "INSERT INTO pkg_script(script_id, package_id, type) " &
              "VALUES ((SELECT script_id FROM script WHERE script = ?1), ?2, ?3)";
         when OPTION1 =>
            return "INSERT OR IGNORE INTO option (option) VALUES (?1)";
         when OPTION2 =>
            return "INSERT INTO pkg_option(package_id, option_id, value) " &
              "VALUES (?1, (SELECT option_id FROM option WHERE option = ?2), ?3)";
         when SHLIBS1 =>
            return "INSERT OR IGNORE INTO shlibs(name) VALUES(?1)";
         when SHLIBS_REQD =>
            return "INSERT OR IGNORE INTO pkg_shlibs_required(package_id, shlib_id) " &
              "VALUES (?1, (SELECT id FROM shlibs WHERE name = ?2))";
         when SHLIBS_PROV =>
            return "INSERT OR IGNORE INTO pkg_shlibs_provided(package_id, shlib_id) " &
              "VALUES (?1, (SELECT id FROM shlibs WHERE name = ?2))";
         when ANNOTATE1 =>
            return "INSERT OR IGNORE INTO annotation(annotation) VALUES (?1)";
         when ANNOTATE2 =>
            return "INSERT OR ROLLBACK INTO pkg_annotation(package_id, tag_id, value_id) " &
              "VALUES (?1, (SELECT annotation_id FROM annotation WHERE annotation = ?2)," &
              " (SELECT annotation_id FROM annotation WHERE annotation = ?3))";
         when ANNOTATE_ADD1 =>
            return "INSERT OR IGNORE INTO pkg_annotation(package_id, tag_id, value_id) " &
              "VALUES (" &
              " (SELECT id FROM packages WHERE name = ?1 )," &
              " (SELECT annotation_id FROM annotation WHERE annotation = ?2)," &
              " (SELECT annotation_id FROM annotation WHERE annotation = ?3))";
         when ANNOTATE_DEL1 =>
            return "DELETE FROM pkg_annotation WHERE package_id IN" &
              " (SELECT id FROM packages WHERE name = ?1) AND tag_id IN" &
              " (SELECT annotation_id FROM annotation WHERE annotation = ?2)";
         when ANNOTATE_DEL2 =>
            return "DELETE FROM annotation WHERE annotation_id NOT IN" &
              " (SELECT tag_id FROM pkg_annotation) AND annotation_id NOT IN" &
              " (SELECT value_id FROM pkg_annotation)";
         when CONFLICT =>
            return "INSERT INTO pkg_conflicts(package_id, conflict_id) " &
              "VALUES (?1, (SELECT id FROM packages WHERE name = ?2))";
         when PKG_PROVIDE =>
            return "INSERT INTO pkg_provides(package_id, provide_id) " &
              "VALUES (?1, (SELECT id FROM provides WHERE provide = ?2))";
         when PROVIDE =>
            return "INSERT OR IGNORE INTO provides(provide) VALUES(?1)";
         when UPDATE_DIGEST =>
            return "UPDATE packages SET manifestdigest=?1 WHERE id=?2";
         when CONFIG_FILES =>
            return "INSERT INTO config_files(path, content, package_id) VALUES (?1, ?2, ?3)";
         when UPDATE_CONFIG_FILE =>
            return "UPDATE config_files SET content=?1 WHERE path=?2";
         when PKG_REQUIRE =>
            return "INSERT INTO pkg_requires(package_id, require_id) " &
              "VALUES (?1, (SELECT id FROM requires WHERE require = ?2))";
         when REQUIRE =>
            return "INSERT OR IGNORE INTO requires(require) VALUES(?1)";
      end case;
   end prstmt_text_sql;


   --------------------------------------------------------------------
   --  prstmt_finalize
   --------------------------------------------------------------------
   procedure prstmt_finalize (db : in out struct_pkgdb) is
   begin
      for S in sql_prstmt_index'Range loop
         SQLite.finalize_statement (sql_prepared_statements (S));
         sql_prepared_statements (S) := null;
      end loop;
      db.prstmt_initialized := False;
   end prstmt_finalize;


   --------------------------------------------------------------------
   --  pkgdb_close
   --------------------------------------------------------------------
   procedure pkgdb_close (db : in out struct_pkgdb)
   is
      use type sqlite_h.sqlite3_Access;

      procedure close (position : pkg_repos_crate.Cursor);
      procedure close_out (key : Text; xrepo : in out T_pkg_repo);

      procedure close (position : pkg_repos_crate.Cursor) is
      begin
         db.repos.Update_Element (Position => position,
                                  Process  => close_out'Access);
      end close;

      procedure close_out (key : Text; xrepo : in out T_pkg_repo)
      is
         result : Boolean;
      begin
         result := Repo_Operations.Ops (xrepo.ops_variant).all.repo_close (xrepo, False);
      end close_out;

   begin
      if db.prstmt_initialized then
         prstmt_finalize (db);
      end if;
      if db.sqlite /= null then
         db.repos.Iterate (close'Access);
         db.repos.Clear;
         SQLite.close_database (db.sqlite);
         db.sqlite := null;
      end if;
      SQLite.shutdown_sqlite;
   end pkgdb_close;


   --------------------------------------------------------------------
   --  ERROR_SQLITE
   --------------------------------------------------------------------
   procedure ERROR_SQLITE (db : sqlite_h.sqlite3_Access; func : String; query : String)
   is
      msg : String := "sqlite error while executing " & query &
        " in file core-pkgdb.adb," & func & "(): " & SQLite.get_last_error_message (db);
   begin
      Event.pkg_emit_error (SUS (msg));
   end ERROR_SQLITE;


   --------------------------------------------------------------------
   --  run_transaction
   --------------------------------------------------------------------
   function run_transaction (db : sqlite_h.sqlite3_Access; query : String; savepoint : String)
                             return Boolean
   is
      function joinsql return String;
      function joinsql return String is
      begin
         if IsBlank (savepoint) then
            return query;
         else
            return query & " " & savepoint;
         end if;
      end joinsql;

      stmt : aliased sqlite_h.sqlite3_stmt_Access;
      func : constant String := "run_transaction";
   begin
      Event.pkg_debug (4, "Pkgdb: running '" & joinsql & "'");
      if SQLite.prepare_sql (db, joinsql, stmt'Access) then
         if not SQLite.step_through_statement (stmt => stmt, num_retries => 6) then
            ERROR_SQLITE (db, func, joinsql);
            SQLite.finalize_statement (stmt);
            return False;
         end if;
         SQLite.finalize_statement (stmt);
         return True;
      else
         ERROR_SQLITE (db, func, joinsql);
         return False;
      end if;
   end run_transaction;


   --------------------------------------------------------------------
   --  pkgdb_transaction_begin_sqlite
   --------------------------------------------------------------------
   function pkgdb_transaction_begin_sqlite (db : sqlite_h.sqlite3_Access; savepoint : String)
                                            return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "BEGIN IMMEDIATE TRANSACTION", "");
      else
         return run_transaction (db, "SAVEPOINT", savepoint);
      end if;
   end pkgdb_transaction_begin_sqlite;


   --------------------------------------------------------------------
   --  pkgdb_transaction_commit_sqlite
   --------------------------------------------------------------------
   function pkgdb_transaction_commit_sqlite (db : sqlite_h.sqlite3_Access; savepoint : String)
                                             return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "COMMIT TRANSACTION", "");
      else
         return run_transaction (db, "RELEASE SAVEPOINT", savepoint);
      end if;
   end pkgdb_transaction_commit_sqlite;


   --------------------------------------------------------------------
   --  pkgdb_transaction_rollback_sqlite
   --------------------------------------------------------------------
   function pkgdb_transaction_rollback_sqlite (db : sqlite_h.sqlite3_Access; savepoint : String)
                                               return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, "ROLLBACK TRANSACTION", "");
      else
         return run_transaction (db, "ROLLBACK TO SAVEPOINT", savepoint);
      end if;
   end pkgdb_transaction_rollback_sqlite;


   --------------------------------------------------------------------
   --  pkgdb_transaction_begin
   --------------------------------------------------------------------
   function pkgdb_transaction_begin (db : struct_pkgdb; savepoint : String) return Boolean is
   begin
      return pkgdb_transaction_begin_sqlite (db.sqlite, savepoint);
   end pkgdb_transaction_begin;


   --------------------------------------------------------------------
   --  pkgdb_transaction_commit
   --------------------------------------------------------------------
   function pkgdb_transaction_commit (db : struct_pkgdb; savepoint : String) return Boolean is
   begin
      return pkgdb_transaction_commit_sqlite (db.sqlite, savepoint);
   end pkgdb_transaction_commit;


   --------------------------------------------------------------------
   --  pkgdb_transaction_rollback
   --------------------------------------------------------------------
   function pkgdb_transaction_rollback (db : struct_pkgdb; savepoint : String) return Boolean is
   begin
      return pkgdb_transaction_rollback_sqlite (db.sqlite, savepoint);
   end pkgdb_transaction_rollback;


   --------------------------------------------------------------------
   --  pkgdb_open_all
   --------------------------------------------------------------------
   function pkgdb_open_all (db : in out struct_pkgdb; dbtype : T_pkgdb; reponame : String)
                            return Core.Pkg.Pkg_Error_Type
   is
      dirfd  : Unix.File_Descriptor;
      create : Boolean := False;
      result : Boolean;
   begin
      if SQLite.db_connected (db.sqlite) then
         Event.pkg_emit_error (SUS ("pkgdb_open_all(): database already connected"));
         return EPKG_FATAL;
      end if;

      dirfd := Config.pkg_get_dbdirfd;
      if not Unix.file_connected (dirfd) then
         --  Failed to create, maybe directory tree doesn't exit.
         declare
            dbdir : String := Config.pkg_config_get_string (Config.conf_dbdir);
         begin
            declare
               dbdir_dirname : String := DIR.Containing_Directory (dbdir);
            begin
               DIR.Create_Path (dbdir_dirname);
               create := True;
            end;
         exception
            when others =>
               Event.pkg_emit_error (SUS ("pkgdb_open_all(): failed to create dbdir directories"));
               return EPKG_FATAL;
         end;
         dirfd := Config.pkg_get_dbdirfd;
         if not Unix.file_connected (dirfd) then
            Event.pkg_emit_error (SUS ("pkgdb_open_all(): failed to open dbdir"));
            return EPKG_FATAL;
         end if;
      end if;

      if not create then
         if not Unix.relative_file_readable (dirfd, "local.sqlite") then
            declare
               dbdir : String := Config.pkg_config_get_string (Config.conf_dbdir);
            begin
               if DIR.Exists (dbdir & "/local.sqlite") then
                  Event.pkg_emit_nolocaldb;
                  return EPKG_ENODB;
               elsif not Unix.relative_file_writable (dirfd, ".") then
                  --  If we need to create the db but cannot
                  --  write to it, fail early
                  Event.pkg_emit_nolocaldb;
                  return EPKG_ENODB;
               else
                  create := True;
               end if;
            end;
         end if;
      end if;

      result := SQLite.initialize_sqlite;
      SQLite.pkgdb_syscall_overload;

     -- SQLite.open_sqlite_database_readonly


      return EPKG_FATAL;
   end pkgdb_open_all;


   --------------------------------------------------------------------
   --  vfs_dbdir_open
   --------------------------------------------------------------------
   function vfs_dbdir_open (path : ICS.chars_ptr; flags : IC.int; mode : IC.int) return IC.int
   is
      dfd      : Unix.File_Descriptor := Config.pkg_get_dbdirfd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      fd       : IC.int;
   begin
      newpath := ICS.New_String (basename);
      fd := Unix.C_Openat_Stock (dirfd => IC.int (dfd),
                                 path  => newpath,
                                 flags => flags,
                                 mode  => mode);
      ICS.Free (newpath);
      return fd;
   end vfs_dbdir_open;


   --------------------------------------------------------------------
   --  vfs_dbdir_access
   --------------------------------------------------------------------
   function vfs_dbdir_access (path : ICS.chars_ptr; mode : IC.int) return IC.int
   is
      dfd      : Unix.File_Descriptor := Config.pkg_get_dbdirfd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      res      : IC.int;
   begin
      newpath := ICS.New_String (basename);
      res     := Unix.C_faccessat (dfd  => IC.int (dfd),
                                   path => newpath,
                                   mode => mode,
                                   flag => 0);
      ICS.Free (newpath);
      return res;
   end vfs_dbdir_access;


   --------------------------------------------------------------------
   --  vfs_dbdir_unlink
   --------------------------------------------------------------------
   function vfs_dbdir_unlink (path : ICS.chars_ptr) return IC.int
   is
      dfd      : Unix.File_Descriptor := Config.pkg_get_dbdirfd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      res      : IC.int;
   begin
      newpath := ICS.New_String (basename);
      res     := Unix.C_unlinkat (dfd  => IC.int (dfd),
                                  path => newpath,
                                  flag => 0);
      ICS.Free (newpath);
      return res;
   end vfs_dbdir_unlink;


   --------------------------------------------------------------------
   --  vfs_dbdir_stat
   --------------------------------------------------------------------
   function vfs_dbdir_stat (path : ICS.chars_ptr; sb : Unix.struct_stat_Access) return IC.int
   is
      dfd      : Unix.File_Descriptor := Config.pkg_get_dbdirfd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      res      : IC.int;
   begin
      newpath := ICS.New_String (basename);
      res     := Unix.C_fstatat (dfd  => IC.int (dfd),
                                 path => newpath,
                                 sb   => sb,
                                 flag => 0);
      ICS.Free (newpath);
      return res;
   end vfs_dbdir_stat;


   --------------------------------------------------------------------
   --  vfs_dbdir_lstat
   --------------------------------------------------------------------
   function vfs_dbdir_lstat (path : ICS.chars_ptr; sb : Unix.struct_stat_Access) return IC.int
   is
      dfd      : Unix.File_Descriptor := Config.pkg_get_dbdirfd;
      basename : String := tail (ICS.Value (path), "/");
   begin
      if Unix.lstatat (dfd, basename, sb) then
         return IC.int (0);
      else
         return IC.int (-1);
      end if;
   end vfs_dbdir_lstat;


   --------------------------------------------------------------------
   --  vfs_dbdir_mkdir
   --------------------------------------------------------------------
   function vfs_dbdir_mkdir (path : ICS.chars_ptr; mode : IC.int) return IC.int
   is
      dfd      : Unix.File_Descriptor := Config.pkg_get_dbdirfd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      res      : IC.int;
   begin
      newpath := ICS.New_String (basename);
      res := Unix.C_mkdirat (dfd  => IC.int (dfd),
                             path => newpath,
                             mode => IC.int (mode));
      ICS.Free (newpath);
      return res;
   end vfs_dbdir_mkdir;

end Core.PkgDB;
