--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Strings;
with Core.Event;
with Core.CommonSQL;
with SQLite;

use Core.Strings;

package body Core.Database.Operations.Schema is

   --------------------------------------------------------------------
   --  prstmt_text_argtypes
   --------------------------------------------------------------------
   function prstmt_text_argtypes (index : prstmt_index) return String is
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
   function prstmt_text_sql (index : prstmt_index) return String is
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
   --  exec
   --------------------------------------------------------------------
   function exec (db : sqlite_h.sqlite3_Access; sql : String) return Action_Result
   is
      msg : Text;
   begin
      Event.emit_debug (4, "rdb: executing '" & sql & "'");
      if SQLite.exec_sql (db, sql, msg) then
         return RESULT_OK;
      else
         Event.emit_error ("Schema.exec() error: " & USS (msg));
         return RESULT_FATAL;
      end if;
   end exec;

      --------------------------------------------------------------------
   --  prstmt_finalize
   --------------------------------------------------------------------
   procedure prstmt_finalize (db : in out RDB_Connection) is
   begin
      for S in Schema.prstmt_index'Range loop
         SQLite.finalize_statement (prepared_statements (S));
         prepared_statements (S) := null;
      end loop;
      db.prstmt_initialized := False;
   end prstmt_finalize;


   --------------------------------------------------------------------
   --  prstmt_initialize
   --------------------------------------------------------------------
   function prstmt_initialize (db : in out RDB_Connection) return Action_Result is
   begin
      if not db.prstmt_initialized then
         for S in prstmt_index'Range loop
            Event.emit_debug
              (4, "Pkgdb: preparing statement '" & prstmt_text_sql (S) & "'");
            if not SQLite.prepare_sql (pDB    => db.sqlite,
                                       sql    => prstmt_text_sql (S),
                                       ppStmt => prepared_statements (S)'Access)
            then
               CommonSQL.ERROR_SQLITE (db.sqlite, "prstmt_initialize", prstmt_text_sql (S));
               return RESULT_FATAL;
            end if;
            db.prstmt_initialized := True;
         end loop;
      end if;
      return RESULT_OK;
   end prstmt_initialize;


   --------------------------------------------------------------------
   --  import_schema_34
   --------------------------------------------------------------------
   function import_schema_34 (db : sqlite_h.sqlite3_Access) return Action_Result
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

        "PRAGMA user_version = 34;" &
        "COMMIT;";

   begin
      return exec (db, sql);
   end import_schema_34;


   --------------------------------------------------------------------
   --  rdb_upgrade
   --------------------------------------------------------------------
   function rdb_upgrade (db : RDB_Connection) return Action_Result
   is
      cur_dbver : int64;
      exp_dbver : constant int64 := int64 (DB_SCHEMA_ALL);
   begin
      if CommonSQL.get_pragma (db      => db.sqlite,
                               sql     => "PRAGMA user_version;",
                               res     => cur_dbver,
                               silence => False) /= RESULT_OK
      then
         return RESULT_FATAL;
      end if;

      if cur_dbver = exp_dbver then
         return RESULT_OK;
      end if;

      --  Make sure database version is at least equal to 34.
      --  If it's less than that, there's corruption
      if cur_dbver < int64 (Upgrade_Series'First - 1) then
         Event.emit_error
           ("FATAL: database version " & int2str (Integer (cur_dbver)) & "is impossibly low");
         return RESULT_FATAL;
      end if;

      --  Rare case where database version on file is greater than maximum known version
      --  This should only happen with developers
      if cur_dbver > exp_dbver then
         if (cur_dbver / 1000) <= int64 (DB_SCHEMA_MAJOR) then
            Event.emit_error
              ("warning: database version " & int2str (Integer (cur_dbver)) &
                 " is newer than the latest " & progname & "version " & DBVERSION &
                 ", but still compatible");
            return RESULT_OK;
         else
            Event.emit_error
              ("Fatal: database version " & int2str (Integer (cur_dbver)) &
                 " is newer than and incompatible with the latest " & progname &
                 "version " & DBVERSION);
            return RESULT_FATAL;
         end if;
      end if;

      if SQLite.database_was_opened_readonly (db.sqlite, "main") then
         Event.emit_error ("The database is outdated and opened readonly");
         return RESULT_FATAL;
      end if;

      --  Start the update
      for step in Upgrade_Series (cur_dbver + 1) .. Upgrade_Series'Last loop
         if not CommonSQL.transaction_begin (db.sqlite, "") then
            Event.emit_error ("schema update transaction begin failed");
            return RESULT_FATAL;
         end if;

         declare
            msg     : Text;
            sql     : constant String := upgrade_sql_for_version (step);
            pragsql : constant String := "PRAGMA user_version = " & int2str (Natural (step));
         begin
            if not SQLite.exec_sql (db.sqlite, sql, msg) then
               Event.emit_error ("schema update failed, sql: " & sql);
               if not CommonSQL.transaction_rollback (db.sqlite, "") then
                  null;
               end if;
               return RESULT_FATAL;
            end if;

            if not SQLite.exec_sql (db.sqlite, pragsql, msg) then
               Event.emit_error ("schema update failed, sql: " & pragsql);
               if not CommonSQL.transaction_rollback (db.sqlite, "") then
                  null;
               end if;
               return RESULT_FATAL;
            end if;
         end;

         if not CommonSQL.transaction_commit (db.sqlite, "") then
            Event.emit_error ("schema update transaction commit failed");
            return RESULT_FATAL;
         end if;
      end loop;

      return RESULT_OK;
   end rdb_upgrade;


   --------------------------------------------------------------------
   --  upgrade_sql_for_version
   --------------------------------------------------------------------
   function upgrade_sql_for_version (version : Upgrade_Series) return String is
   begin
      case version is
         when 35 =>
            return
              "CREATE TABLE lua_script("
              & "  lua_script_id INTEGER PRIMARY KEY,"
              & "  lua_script TEXT NOT NULL UNIQUE"
              & ");"
              & "CREATE TABLE pkg_lua_script ("
              & "  package_id INTEGER NOT NULL REFERENCES packages(id)"
              & "    ON DELETE CASCADE ON UPDATE CASCADE,"
              & "  lua_script_id INTEGER NOT NULL REFERENCES lua_script(lua_script_id)"
              & "    ON DELETE RESTRICT ON UPDATE RESTRICT,"
              & "  type INTEGER,"
              & "  UNIQUE(package_id, lua_script_id)"
              & ");"
              & "CREATE VIEW lua_scripts AS "
              & "  SELECT package_id, lua_script, type "
              & "  FROM pkg_lua_script JOIN lua_script USING(lua_script_id);"
              & "CREATE TRIGGER lua_script_update "
              & "  INSTEAD OF UPDATE ON lua_scripts "
              & "FOR EACH ROW BEGIN "
              & "  UPDATE pkg_lua_script "
              & "  SET type = new.type "
              & "  WHERE package_id = old.package_id AND "
              & "  lua_script_id = (SELECT lua_script_id FROM lua_script "
              & "    WHERE lua_script = old.lua_script );"
              & "END;"
              & "CREATE TRIGGER lua_script_insert "
              & "  INSTEAD OF INSERT ON lua_scripts "
              & "FOR EACH ROW BEGIN "
              & "  INSERT OR IGNORE INTO lua_script(lua_script) "
              & "  VALUES(new.lua_script);"
              & "  INSERT INTO pkg_lua_script(package_id, lua_script_id, type) "
              & "  VALUES (new.package_id, "
              & "    (SELECT lua_script_id FROM lua_script "
              & "    WHERE lua_script = new.lua_script), "
              & "    new.type);"
              & "END;"
              & "CREATE TRIGGER lua_script_delete "
              & "  INSTEAD OF DELETE ON lua_scripts "
              & "FOR EACH ROW BEGIN "
              & "  DELETE FROM pkg_lua_script "
              & "  WHERE package_id = old.package_id AND "
              & "    lua_script_id = ( SELECT lua_script_id FROM lua_script "
              & "                      WHERE lua_script = old.lua_script );"
              & "  DELETE FROM lua_script "
              & "  WHERE lua_script_id NOT IN "
              & "    ( SELECT DISTINCT lua_script_id from lua_script );"
              & "END;";
      end case;
   end upgrade_sql_for_version;


end Core.Database.Operations.Schema;
