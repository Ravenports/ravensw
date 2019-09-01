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
      pzErrMsg : access ICS.chars_ptr;
      pThunk   : sqlite_h.sqlite3_api_routines_Access) return IC.int
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
   --  prstmt_initialize
   --------------------------------------------------------------------
   function prstmt_initialize (db : in out struct_pkgdb) return Pkg_Error_Type is
   begin
      if not db.prstmt_initialized then
         for S in sql_prstmt_index'Range loop
            Event.pkg_debug (4, "Pkgdb: preparing statement '" & prstmt_text_sql (S) & "'");
            if not SQLite.prepare_sql (pDB    => db.sqlite,
                                       sql    => prstmt_text_sql (S),
                                       ppStmt => sql_prepared_statements (S)'Access)
            then
               ERROR_SQLITE (db.sqlite, "prstmt_initialize", prstmt_text_sql (S));
               return (EPKG_FATAL);
            end if;
            db.prstmt_initialized := True;
         end loop;
      end if;
      return (EPKG_OK);
   end prstmt_initialize;


   --------------------------------------------------------------------
   --  pkgdb_close
   --------------------------------------------------------------------
   procedure pkgdb_close (db : in out struct_pkgdb)
   is
      use type sqlite_h.sqlite3_Access;

      procedure close (position : text_crate.Cursor);
      procedure close (position : text_crate.Cursor)
      is
         result   : Boolean;
         reponame : Text renames text_crate.Element (position);
         variant  : repo_ops_variant := Config.repositories.Element (reponame).ops_variant;
      begin
         result := Repo_Operations.Ops (variant).all.repo_close (reponame, False);
      end close;

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
      use type sqlite_h.enum_error_types;
      func   : constant String := "pkgdb_open_all";
      dirfd  : Unix.File_Descriptor;
      create : Boolean := False;
      result : Boolean;
   begin
      if SQLite.db_connected (db.sqlite) then
         --  database is already open, just load another repository and exit
         return pkgdb_open_remote (db, dbtype, reponame);
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


      if not SQLite.open_sqlite_database_readwrite ("/local.sqlite", db.sqlite'Access) then
         ERROR_SQLITE (db.sqlite, func, "sqlite open");
         if SQLite.get_last_error_code (db.sqlite) = sqlite_h.SQLITE_CORRUPT then
            Event.pkg_emit_error (SUS ("Database corrupt.  Are you running on NFS?  " &
                                    "If so, ensure the locking mechanism is properly set up."));
         end if;
         pkgdb_close (db);
         return EPKG_FATAL;
      end if;

      --  Wait up to 5 seconds if database is busy
      declare
         use type IC.int;
         res : IC.int;
      begin
         res := sqlite_h.sqlite3_busy_timeout (db.sqlite, IC.int (5000));
         if res /= 0 then
            Event.pkg_emit_error (SUS ("Failed to set busy timeout in " & func));
         end if;
      end;

      --  If the database is missing we have to initialize it
      if create and then
        pkgdb_init (db.sqlite) /= EPKG_OK
      then
         pkgdb_close (db);
         return EPKG_FATAL;
      end if;

      --  Create our functions
      declare
         use type IC.int;
         res : IC.int;
      begin
         res := pkgdb_sqlcmd_init (db       => db.sqlite,
                                   pzErrMsg => null,
                                   pThunk   => null);
         if res /= 0 then
            Event.pkg_emit_error (SUS ("Failed to add custom sql functions in " & func));
            pkgdb_close (db);
            return EPKG_FATAL;
         end if;
      end;

      if pkgdb_upgrade (db) /= EPKG_OK then
         --  pkgdb_upgrade() emits error events; we don't need to add more
         pkgdb_close (db);
         return EPKG_FATAL;
      end if;

      --  allow foreign key option which will allow to have
      --  clean support for reinstalling
      declare
         msg : Text;
         sql : constant String := "PRAGMA foreign_keys = ON";
      begin
         if not SQLite.exec_sql (db.sqlite, sql, msg) then
            ERROR_SQLITE (db.sqlite, func, sql);
            pkgdb_close (db);
            return EPKG_FATAL;
         end if;
      end;

      declare
         result : Pkg_Error_Type;
      begin
         result := pkgdb_open_remote (db, dbtype, reponame);
         if result /= EPKG_OK then
            return result;
         end if;
      end;

      if prstmt_initialize (db) /= EPKG_OK then
         Event.pkg_emit_error (SUS ("Failed to initialize prepared statements"));
         pkgdb_close (db);
         return EPKG_FATAL;
      end if;

      if Config.pkg_config_get_boolean (Config.conf_sqlite_profile) then
         Event.pkg_debug (1, "pkgdb profiling is enabled");
         SQLite.set_sqlite_profile (db.sqlite, pkgdb_profile_callback'Access);
      end if;

      return EPKG_OK;
   end pkgdb_open_all;


   --------------------------------------------------------------------
   --  pkgdb_open_remote
   --------------------------------------------------------------------
   function pkgdb_open_remote (db : in out struct_pkgdb; dbtype : T_pkgdb; reponame : String)
                               return Core.Pkg.Pkg_Error_Type
   is
   begin
      if dbtype = PKGDB_REMOTE or else dbtype = PKGDB_MAYBE_REMOTE then
         if reponame /= "" then
            if Config.pkg_repo_is_active (reponame) then
               declare
                  ret : Pkg_Error_Type;
               begin
                  ret := pkgdb_open_repository (db, reponame);
                  if ret /= EPKG_OK then
                     Event.pkg_emit_error (SUS ("Failed to open repository " & reponame));
                     pkgdb_close (db);
                     return EPKG_FATAL;
                  end if;
               end;
            else
               Event.pkg_emit_error
                 (SUS ("Repository " & reponame & " is not active or does not exist"));
               pkgdb_close (db);
               return EPKG_FATAL;
            end if;
         elsif Config.pkg_repos_activated_count > 0 then
            declare
               ret     : Pkg_Error_Type;
               farname : String := Config.first_active_repository;
            begin
               ret := pkgdb_open_repository (db, farname);
               if ret /= EPKG_OK then
                  Event.pkg_emit_error (SUS ("Failed to open first active repository " & farname));
                  pkgdb_close (db);
                  return EPKG_FATAL;
               end if;
            end;
         elsif dbtype = PKGDB_REMOTE then
            Event.pkg_emit_error (SUS ("No active remote repositories configured"));
            pkgdb_close (db);
            return EPKG_FATAL;
         end if;
      end if;

      return EPKG_OK;
   end pkgdb_open_remote;


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


   --------------------------------------------------------------------
   --  pkgdb_init
   --------------------------------------------------------------------
   function pkgdb_init (db : sqlite_h.sqlite3_Access) return Core.Pkg.Pkg_Error_Type
   is
      sql : constant String :=
        "BEGIN;" &
        "CREATE TABLE packages (" &
                "id INTEGER PRIMARY KEY," &
                "origin TEXT NOT NULL," &
                "name TEXT NOT NULL," &
                "version TEXT NOT NULL," &
                "comment TEXT NOT NULL," &
                "desc TEXT NOT NULL," &
                "mtree_id INTEGER REFERENCES mtree(id)" &
                        " ON DELETE RESTRICT ON UPDATE CASCADE," &
                "message TEXT," &
                "arch TEXT NOT NULL," &
                "maintainer TEXT NOT NULL," &
                "www TEXT," &
                "prefix TEXT NOT NULL," &
                "flatsize INTEGER NOT NULL," &
                "automatic INTEGER NOT NULL," &
                "locked INTEGER NOT NULL DEFAULT 0," &
                "licenselogic INTEGER NOT NULL," &
                "time INTEGER, " &
                "manifestdigest TEXT NULL, " &
                "pkg_format_version INTEGER," &
                "dep_formula TEXT NULL," &
                "vital INTEGER NOT NULL DEFAULT 0" &
        ");" &
        "CREATE UNIQUE INDEX packages_unique ON packages(name);" &
        "CREATE TABLE mtree (" &
                "id INTEGER PRIMARY KEY," &
                "content TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_script (" &
                "package_id INTEGER REFERENCES packages(id)" &
                        " ON DELETE CASCADE ON UPDATE CASCADE," &
                "type INTEGER," &
                "script_id INTEGER REFERENCES script(script_id)" &
                        " ON DELETE RESTRICT ON UPDATE CASCADE," &
                "PRIMARY KEY (package_id, type)" &
        ");" &
        "CREATE TABLE script (" &
                "script_id INTEGER PRIMARY KEY," &
                "script TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE option (" &
                "option_id INTEGER PRIMARY KEY," &
                "option TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE option_desc (" &
                "option_desc_id INTEGER PRIMARY KEY," &
                "option_desc TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_option (" &
                "package_id INTEGER NOT NULL REFERENCES packages(id) " &
                        "ON DELETE CASCADE ON UPDATE CASCADE," &
                "option_id INTEGER NOT NULL REFERENCES option(option_id) " &
                        "ON DELETE RESTRICT ON UPDATE CASCADE," &
                "value TEXT NOT NULL," &
                "PRIMARY KEY(package_id, option_id)" &
        ");" &
        "CREATE TABLE pkg_option_desc (" &
                "package_id INTEGER NOT NULL REFERENCES packages(id) " &
                        "ON DELETE CASCADE ON UPDATE CASCADE," &
                "option_id INTEGER NOT NULL REFERENCES option(option_id) " &
                        "ON DELETE RESTRICT ON UPDATE CASCADE," &
                "option_desc_id INTEGER NOT NULL " &
                        "REFERENCES option_desc(option_desc_id) " &
                        "ON DELETE RESTRICT ON UPDATE CASCADE," &
                "PRIMARY KEY(package_id, option_id)" &
        ");" &
        "CREATE TABLE pkg_option_default (" &
                "package_id INTEGER NOT NULL REFERENCES packages(id) " &
                        "ON DELETE CASCADE ON UPDATE CASCADE," &
                "option_id INTEGER NOT NULL REFERENCES option(option_id) " &
                        "ON DELETE RESTRICT ON UPDATE CASCADE," &
                "default_value TEXT NOT NULL," &
                "PRIMARY KEY(package_id, option_id)" &
        ");" &
        "CREATE TABLE deps (" &
                "origin TEXT NOT NULL," &
                "name TEXT NOT NULL," &
                "version TEXT NOT NULL," &
                "package_id INTEGER REFERENCES packages(id) ON DELETE CASCADE" &
                        " ON UPDATE CASCADE" &
        ");" &
        "CREATE UNIQUE INDEX deps_unique ON deps(name, version, package_id);" &
        "CREATE TABLE files (" &
                "path TEXT PRIMARY KEY," &
                "sha256 TEXT," &
                "package_id INTEGER REFERENCES packages(id) ON DELETE CASCADE" &
                        " ON UPDATE CASCADE" &
        ");" &
        "CREATE TABLE directories (" &
                "id INTEGER PRIMARY KEY," &
                "path TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_directories (" &
                "package_id INTEGER REFERENCES packages(id) ON DELETE CASCADE" &
                        " ON UPDATE CASCADE," &
                "directory_id INTEGER REFERENCES directories(id) ON DELETE RESTRICT" &
                        " ON UPDATE RESTRICT," &
                "try INTEGER," &
                "PRIMARY KEY (package_id, directory_id)" &
        ");" &
        "CREATE TABLE categories (" &
                "id INTEGER PRIMARY KEY," &
                "name TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_categories (" &
                "package_id INTEGER REFERENCES packages(id) ON DELETE CASCADE" &
                        " ON UPDATE CASCADE," &
                "category_id INTEGER REFERENCES categories(id) ON DELETE RESTRICT" &
                        " ON UPDATE RESTRICT," &
                "PRIMARY KEY (package_id, category_id)" &
        ");" &
        "CREATE TABLE licenses (" &
                "id INTEGER PRIMARY KEY," &
                "name TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_licenses (" &
                "package_id INTEGER REFERENCES packages(id) ON DELETE CASCADE" &
                        " ON UPDATE CASCADE," &
                "license_id INTEGER REFERENCES licenses(id) ON DELETE RESTRICT" &
                        " ON UPDATE RESTRICT," &
                "PRIMARY KEY (package_id, license_id)" &
        ");" &
        "CREATE TABLE users (" &
                "id INTEGER PRIMARY KEY," &
                "name TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_users (" &
                "package_id INTEGER REFERENCES packages(id) ON DELETE CASCADE" &
                        " ON UPDATE CASCADE," &
                "user_id INTEGER REFERENCES users(id) ON DELETE RESTRICT" &
                        " ON UPDATE RESTRICT," &
                "UNIQUE(package_id, user_id)" &
        ");" &
        "CREATE TABLE groups (" &
                "id INTEGER PRIMARY KEY," &
                "name TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_groups (" &
                "package_id INTEGER REFERENCES packages(id) ON DELETE CASCADE" &
                        " ON UPDATE CASCADE," &
                "group_id INTEGER REFERENCES groups(id) ON DELETE RESTRICT" &
                        " ON UPDATE RESTRICT," &
                "UNIQUE(package_id, group_id)" &
        ");" &
        "CREATE TABLE shlibs (" &
                "id INTEGER PRIMARY KEY," &
                "name TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_shlibs_required (" &
                "package_id INTEGER NOT NULL REFERENCES packages(id)" &
                        " ON DELETE CASCADE ON UPDATE CASCADE," &
                "shlib_id INTEGER NOT NULL REFERENCES shlibs(id)" &
                        " ON DELETE RESTRICT ON UPDATE RESTRICT," &
                "UNIQUE (package_id, shlib_id)" &
        ");" &
        "CREATE TABLE pkg_shlibs_provided (" &
                "package_id INTEGER NOT NULL REFERENCES packages(id)" &
                        " ON DELETE CASCADE ON UPDATE CASCADE," &
                "shlib_id INTEGER NOT NULL REFERENCES shlibs(id)" &
                        " ON DELETE RESTRICT ON UPDATE RESTRICT," &
                "UNIQUE (package_id, shlib_id)" &
        ");" &
        "CREATE TABLE annotation (" &
                "annotation_id INTEGER PRIMARY KEY," &
                "annotation TEXT NOT NULL UNIQUE" &
        ");" &
        "CREATE TABLE pkg_annotation (" &
                "package_id INTERGER REFERENCES packages(id)" &
                      " ON DELETE CASCADE ON UPDATE RESTRICT," &
                "tag_id INTEGER NOT NULL REFERENCES annotation(annotation_id)" &
                      " ON DELETE CASCADE ON UPDATE RESTRICT," &
                "value_id INTEGER NOT NULL REFERENCES annotation(annotation_id)" &
                      " ON DELETE CASCADE ON UPDATE RESTRICT," &
                "UNIQUE (package_id, tag_id)" &
        ");" &
        "CREATE TABLE pkg_conflicts (" &
            "package_id INTEGER NOT NULL REFERENCES packages(id)" &
            "  ON DELETE CASCADE ON UPDATE CASCADE," &
            "conflict_id INTEGER NOT NULL," &
            "UNIQUE(package_id, conflict_id)" &
        ");" &
        "CREATE TABLE pkg_lock (" &
            "exclusive INTEGER(1)," &
            "advisory INTEGER(1)," &
            "read INTEGER(8)" &
        ");" &
        "CREATE TABLE pkg_lock_pid (" &
            "pid INTEGER PRIMARY KEY" &
        ");" &
        "INSERT INTO pkg_lock VALUES(0,0,0);" &
        "CREATE TABLE provides(" &
        "    id INTEGER PRIMARY KEY," &
        "    provide TEXT NOT NULL" &
        ");" &
        "CREATE TABLE pkg_provides (" &
            "package_id INTEGER NOT NULL REFERENCES packages(id)" &
            "  ON DELETE CASCADE ON UPDATE CASCADE," &
            "provide_id INTEGER NOT NULL REFERENCES provides(id)" &
            "  ON DELETE RESTRICT ON UPDATE RESTRICT," &
            "UNIQUE(package_id, provide_id)" &
        ");" &
        "CREATE TABLE config_files (" &
                "path TEXT NOT NULL UNIQUE, " &
                "content TEXT, " &
                "package_id INTEGER REFERENCES packages(id) ON DELETE CASCADE" &
                        " ON UPDATE CASCADE" &
        ");" &

        "CREATE INDEX deporigini on deps(origin);" &
        "CREATE INDEX pkg_script_package_id ON pkg_script(package_id);" &
        "CREATE INDEX deps_package_id ON deps (package_id);" &
        "CREATE INDEX files_package_id ON files (package_id);" &
        "CREATE INDEX pkg_directories_package_id ON pkg_directories (package_id);" &
        "CREATE INDEX pkg_categories_package_id ON pkg_categories (package_id);" &
        "CREATE INDEX pkg_licenses_package_id ON pkg_licenses (package_id);" &
        "CREATE INDEX pkg_users_package_id ON pkg_users (package_id);" &
        "CREATE INDEX pkg_groups_package_id ON pkg_groups (package_id);" &
        "CREATE INDEX pkg_shlibs_required_package_id ON pkg_shlibs_required (package_id);" &
        "CREATE INDEX pkg_shlibs_provided_package_id ON pkg_shlibs_provided (package_id);" &
        "CREATE INDEX pkg_directories_directory_id ON pkg_directories (directory_id);" &
        "CREATE INDEX pkg_annotation_package_id ON pkg_annotation(package_id);" &
        "CREATE INDEX pkg_digest_id ON packages(origin, manifestdigest);" &
        "CREATE INDEX pkg_conflicts_pid ON pkg_conflicts(package_id);" &
        "CREATE INDEX pkg_conflicts_cid ON pkg_conflicts(conflict_id);" &
        "CREATE INDEX pkg_provides_id ON pkg_provides(package_id);" &
        "CREATE INDEX packages_origin ON packages(origin COLLATE NOCASE);" &
        "CREATE INDEX packages_name ON packages(name COLLATE NOCASE);" &

        "CREATE VIEW pkg_shlibs AS SELECT * FROM pkg_shlibs_required;" &
        "CREATE TRIGGER pkg_shlibs_update " &
                "INSTEAD OF UPDATE ON pkg_shlibs " &
        "FOR EACH ROW BEGIN " &
                "UPDATE pkg_shlibs_required " &
                "SET package_id = new.package_id, " &
                "  shlib_id = new.shlib_id " &
                "WHERE shlib_id = old.shlib_id " &
                "AND package_id = old.package_id; " &
        "END;" &
        "CREATE TRIGGER pkg_shlibs_insert " &
                "INSTEAD OF INSERT ON pkg_shlibs " &
        "FOR EACH ROW BEGIN " &
                "INSERT INTO pkg_shlibs_required (shlib_id, package_id) " &
                "VALUES (new.shlib_id, new.package_id); " &
        "END;" &
        "CREATE TRIGGER pkg_shlibs_delete " &
                "INSTEAD OF DELETE ON pkg_shlibs " &
        "FOR EACH ROW BEGIN " &
                "DELETE FROM pkg_shlibs_required " &
                "WHERE shlib_id = old.shlib_id " &
                "AND package_id = old.package_id; " &
        "END;" &

        "CREATE VIEW scripts AS SELECT package_id, script, type" &
                " FROM pkg_script ps JOIN script s" &
                " ON (ps.script_id = s.script_id);" &
        "CREATE TRIGGER scripts_update" &
                " INSTEAD OF UPDATE ON scripts " &
        "FOR EACH ROW BEGIN" &
                " INSERT OR IGNORE INTO script(script)" &
                " VALUES(new.script);" &
                " UPDATE pkg_script" &
                " SET package_id = new.package_id," &
                        " type = new.type," &
                        " script_id = ( SELECT script_id" &
                        " FROM script WHERE script = new.script )" &
                " WHERE package_id = old.package_id" &
                        " AND type = old.type;" &
        "END;" &
        "CREATE TRIGGER scripts_insert" &
                " INSTEAD OF INSERT ON scripts " &
        "FOR EACH ROW BEGIN" &
                " INSERT OR IGNORE INTO script(script)" &
                " VALUES(new.script);" &
                " INSERT INTO pkg_script(package_id, type, script_id) " &
                " SELECT new.package_id, new.type, s.script_id" &
                " FROM script s WHERE new.script = s.script;" &
        "END;" &
        "CREATE TRIGGER scripts_delete" &
                " INSTEAD OF DELETE ON scripts " &
        "FOR EACH ROW BEGIN" &
                " DELETE FROM pkg_script" &
                " WHERE package_id = old.package_id" &
                " AND type = old.type;" &
                " DELETE FROM script" &
                " WHERE script_id NOT IN" &
                         " (SELECT DISTINCT script_id FROM pkg_script);" &
        "END;" &
        "CREATE VIEW options AS " &
                "SELECT package_id, option, value " &
                "FROM pkg_option JOIN option USING(option_id);" &
        "CREATE TRIGGER options_update " &
                "INSTEAD OF UPDATE ON options " &
        "FOR EACH ROW BEGIN " &
                "UPDATE pkg_option " &
                "SET value = new.value " &
                "WHERE package_id = old.package_id AND " &
                        "option_id = ( SELECT option_id FROM option " &
                                      "WHERE option = old.option );" &
        "END;" &
        "CREATE TRIGGER options_insert " &
                "INSTEAD OF INSERT ON options " &
        "FOR EACH ROW BEGIN " &
                "INSERT OR IGNORE INTO option(option) " &
                "VALUES(new.option);" &
                "INSERT INTO pkg_option(package_id, option_id, value) " &
                "VALUES (new.package_id, " &
                        "(SELECT option_id FROM option " &
                        "WHERE option = new.option), " &
                        "new.value);" &
        "END;" &
        "CREATE TRIGGER options_delete " &
                "INSTEAD OF DELETE ON options " &
        "FOR EACH ROW BEGIN " &
                "DELETE FROM pkg_option " &
                "WHERE package_id = old.package_id AND " &
                        "option_id = ( SELECT option_id FROM option " &
                                        "WHERE option = old.option );" &
                "DELETE FROM option " &
                "WHERE option_id NOT IN " &
                        "( SELECT DISTINCT option_id FROM pkg_option );" &
        "END;" &
        "CREATE TABLE requires(" &
        "    id INTEGER PRIMARY KEY," &
        "    require TEXT NOT NULL" &
        ");" &
        "CREATE TABLE pkg_requires (" &
            "package_id INTEGER NOT NULL REFERENCES packages(id)" &
            "  ON DELETE CASCADE ON UPDATE CASCADE," &
            "require_id INTEGER NOT NULL REFERENCES requires(id)" &
            "  ON DELETE RESTRICT ON UPDATE RESTRICT," &
            "UNIQUE(package_id, require_id)" &
        ");" &

        "PRAGMA user_version = " & DBVERSION & ";" &
        "COMMIT;";

   begin
      return sql_exec (db, sql);
   end pkgdb_init;


   --------------------------------------------------------------------
   --  sql_exec
   --------------------------------------------------------------------
   function sql_exec (db : sqlite_h.sqlite3_Access; sql : String) return Core.Pkg.Pkg_Error_Type
   is
      msg : Text;
   begin
      Event.pkg_debug (4, "Pkgdb: executing '" & sql & "'");
      if SQLite.exec_sql (db, sql, msg) then
         return Core.Pkg.EPKG_OK;
      else
         Event.pkg_emit_error (SUS ("sql_exec() error: " & USS (msg)));
         return Core.Pkg.EPKG_FATAL;
      end if;
   end sql_exec;


   --------------------------------------------------------------------
   --  sql_exec
   --------------------------------------------------------------------
   function get_pragma (db      : sqlite_h.sqlite3_Access;
                        sql     : String;
                        res     : out SQLite.sql_int64;
                        silence : Boolean) return Core.Pkg.Pkg_Error_Type
   is
      stmt : aliased sqlite_h.sqlite3_stmt_Access;
      func : constant String := "get_pragma";
   begin
      res := 0;
      Event.pkg_debug (4, "Pkgdb: executing pragma command '" & sql & "'");
      if not SQLite.prepare_sql (db, sql, stmt'Access) then
         if not silence then
            ERROR_SQLITE (db, func, sql);
         end if;
         return EPKG_FATAL;
      end if;

      if not SQLite.step_through_statement (stmt => stmt, num_retries => 6) then
         SQLite.finalize_statement (stmt);
         Event.pkg_emit_error (SUS ("Pkgdb: failed to step through get_pragma()"));
         return EPKG_FATAL;
      end if;

      res := SQLite.retrieve_integer (stmt, 0);
      SQLite.finalize_statement (stmt);

      return EPKG_OK;
   end get_pragma;


   --------------------------------------------------------------------
   --  pkgdb_upgrade
   --------------------------------------------------------------------
   function pkgdb_upgrade (db : struct_pkgdb) return Core.Pkg.Pkg_Error_Type
   is
      use type SQLite.sql_int64;
      cur_dbver : SQLite.sql_int64;
      exp_dbver : constant SQLite.sql_int64 := SQLite.sql_int64 (Natural'Value (DBVERSION));
   begin
      if get_pragma (db.sqlite, "PRAGMA user_version;", cur_dbver, False) /= EPKG_OK then
         return EPKG_FATAL;
      end if;

      if cur_dbver = exp_dbver then
         return EPKG_OK;
      end if;

      if cur_dbver > exp_dbver then
         if Natural (cur_dbver / 1000) <= DB_SCHEMA_MAJOR then
            --  VIEWS and TRIGGERS used as compatibility hack
            Event.pkg_emit_error
              (SUS ("warning: database version " & int2str (Integer (cur_dbver)) &
                 " is newer than the latest " & progname & "version " & DBVERSION &
                 ", but still compatible"));
            return EPKG_OK;
         else
            Event.pkg_emit_error
              (SUS ("database version " & int2str (Integer (cur_dbver)) &
                 " is newer than and incompatible with the latest " & progname &
                 "version " & DBVERSION));
            return EPKG_FATAL;
         end if;
      end if;

      if SQLite.database_was_opened_readonly (db.sqlite, "main") then
         Event.pkg_emit_error (SUS ("The database is outdated and opened readonly"));
         return EPKG_FATAL;
      end if;

      --  We only need to check availability of upgrade once.  If the lowest version is
      --  supported, all subsequent versions will be as well.
      if not upgrade_available (Natural (cur_dbver) + 1) then
         Event.pkg_emit_error (SUS ("Upgrade support has been removed for ancient " &
                                 "database version " & int2str (Integer (cur_dbver))));
         return EPKG_FATAL;
      end if;

      loop
         exit when cur_dbver = exp_dbver;
         cur_dbver := cur_dbver + 1;

         if not pkgdb_transaction_begin_sqlite (db.sqlite, "") then
            Event.pkg_emit_error (SUS ("pkgdb_upgrade() transaction start failed"));
            return EPKG_FATAL;
         end if;

         declare
            msg     : Text;
            sql     : constant String := upgrade_sql_for_next_version (Natural (cur_dbver));
            pragsql : constant String := "PRAGMA user_version = " &
                                         int2str (Natural (cur_dbver));
         begin
            if not SQLite.exec_sql (db.sqlite, sql, msg) then
               Event.pkg_emit_error (SUS ("pkgdb_upgrade() failed, sql: " & sql));
               if not pkgdb_transaction_rollback_sqlite (db.sqlite, "") then
                  null;
               end if;
               return EPKG_FATAL;
            end if;

            if not SQLite.exec_sql (db.sqlite, pragsql, msg) then
               Event.pkg_emit_error (SUS ("pkgdb_upgrade() failed, sql: " & pragsql));
               if not pkgdb_transaction_rollback_sqlite (db.sqlite, "") then
                  null;
               end if;
               return EPKG_FATAL;
            end if;
         end;

         if not pkgdb_transaction_commit_sqlite (db.sqlite, "") then
            Event.pkg_emit_error (SUS ("pkgdb_upgrade() transaction commit failed"));
            return EPKG_FATAL;
         end if;
      end loop;

      return EPKG_OK;
   end pkgdb_upgrade;


   --------------------------------------------------------------------
   --  upgrade_available
   --------------------------------------------------------------------
   function upgrade_available (current_version : Natural) return Boolean
   is
      subtype upgrade_range is Natural range 0 .. DB_SCHEMA_MAJOR * 1000 + DB_SCHEMA_MINOR;
   begin
      if current_version > upgrade_range'Last then
         return False;
      end if;
      declare
         curversion : constant upgrade_range := upgrade_range (current_version);
      begin
         case curversion is
            when 33 .. 34 => return True;
            when 0 .. 32  => return False;
         end case;
      end;
   end upgrade_available;


   --------------------------------------------------------------------
   --  upgrade_sql_for_next_version
   --------------------------------------------------------------------
   function upgrade_sql_for_next_version (current_version : Natural) return String
   is
      subtype upgrade_range is Natural range 0 .. DB_SCHEMA_MAJOR * 1000 + DB_SCHEMA_MINOR;
   begin
      if current_version > upgrade_range'Last then
         return "version out-of-range";
      end if;
      declare
         curversion : constant upgrade_range := upgrade_range (current_version);
      begin
         case curversion is
            when 0 .. 32 =>
               return "unsupported";
            when 33 =>
               return "ALTER TABLE packages ADD COLUMN vital INTEGER NOT NULL DEFAULT 0";
            when 34 =>
               return "DROP TABLE pkg_search";
         end case;
      end;
   end upgrade_sql_for_next_version;


   --------------------------------------------------------------------
   --  pkgdb_open_repository
   --------------------------------------------------------------------
   function pkgdb_open_repository (db       : in out struct_pkgdb;
                                   reponame : String) return Core.Pkg.Pkg_Error_Type
   is
      key     : Text := SUS (reponame);
      xrepo   : T_pkg_repo renames Config.repositories.Element (key);
      variant : repo_ops_variant renames xrepo.ops_variant;
   begin
      if Repo_Operations.Ops (variant).all.repo_open
        (reponame => key,
         mode     => Repo_Operations.ACCESS_R_OK)
      then
         if Repo_Operations.Ops (variant).all.repo_init (key)
         then
            db.repos.Prepend (key);
            return EPKG_OK;
         else
            Event.pkg_emit_error (SUS ("Repository " & reponame & "' cannot be initialized."));
            return EPKG_FATAL;
         end if;
      else
         Event.pkg_emit_error (SUS ("Repository " & reponame & " cannot be opened. '" &
                                 progname & " update' required."));
         return EPKG_FATAL;
      end if;
   end pkgdb_open_repository;


   --------------------------------------------------------------------
   --  pkgdb_profile_callback
   --------------------------------------------------------------------
   function pkgdb_profile_callback
     (trace_type : IC.unsigned;
      ud   : sqlite_h.Void_Ptr;
      stmt : sqlite_h.Void_Ptr;
      x    : sqlite_h.Void_Ptr) return IC.int
   is
      use type SQLite.sql_int64;
      nsec        : SQLite.sql_int64;
      nsec_Access : access SQLite.sql_int64;
      stmt_Access : sqlite_h.sqlite3_stmt_Access;

      for nsec_Access'Address use x;
      pragma Import (Ada, nsec_Access);

      for stmt_Access'Address use stmt;
      pragma Import (Ada, stmt_Access);
   begin
      --  According to sqlite3 documentation, nsec has milliseconds accuracy
      nsec := nsec_Access.all / 1_000_000;
      if nsec > 0 then
         Event.pkg_debug (1, "Sqlite request " & SQLite.get_sql (stmt_Access) &
                            " was executed in " & int2str (Integer (nsec)) & " milliseconds");
      end if;
      return 0;
   end pkgdb_profile_callback;


   --------------------------------------------------------------------
   --  pkgdb_profile_callback
   --------------------------------------------------------------------
   function pkgdb_open (db : in out struct_pkgdb; dbtype : T_pkgdb) return Core.Pkg.Pkg_Error_Type
   is
   begin
      return pkgdb_open_all (db, dbtype, "");
   end pkgdb_open;

end Core.PkgDB;
