--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Event;
with Core.CommonSQL;
with SQLite;

package body Core.Repo.Operations.Schema is

   --------------------------------------------------------------------`
   --  get_info
   --------------------------------------------------------------------
   function get_info (version : Upgrade_Series; info_type : field) return String is
   begin
      case version is
         when 2014 =>
            case info_type is
               when summary    => return "Remove full text search feature";
               when SQL_string => return "DROP TABLE pkg_search;";
            end case;
      end case;
   end get_info;


   --------------------------------------------------------------------
   --  repo_upgrade
   --------------------------------------------------------------------
   function repo_upgrade (db : sqlite_h.sqlite3_Access; reponame : String) return Action_Result
   is
      reposcver : int64;
      repomajor : int64;
   begin
      if CommonSQL.get_pragma (db      => db,
                               sql     => "PRAGMA user_version",
                               res     => reposcver,
                               silence => False) /= RESULT_OK
      then
         return RESULT_FATAL;
      end if;

      --  If the local ravensw uses a repo schema behind that used to
      --  create the repo, we may still be able use it for reading
      --  (ie ravensw install), but ravensw repo can't do an incremental
      --  update unless the actual schema matches the compiled in
      --  schema version.
      --
      --  Use a major - minor version schema: as the user_version
      --  PRAGMA takes an integer version, encode this as MAJOR *
      --  1000 + MINOR.
      --
      --  So long as the major versions are the same, the local ravensw
      --  should be compatible with any repo created by a more recent
      --  ravensw, although it may need some modification of the repo schema

      repomajor := reposcver / 1000;

      if reposcver = int64 (REPO_SCHEMA_ALL) then
         return RESULT_UPTODATE;
      end if;

      if reposcver > int64 (REPO_SCHEMA_ALL) then
         if repomajor > int64 (REPO_SCHEMA_MAJOR) then
            Event.emit_error
              ("Repo " & reponame & " (schema version " & int2str (Integer (reposcver)) &
                 " is too new -- the maximum requirement is schema " &
                 int2str (((REPO_SCHEMA_MAJOR + 1) * 1000) - 1));
            return RESULT_REPOSCHEMA;
         end if;
         return RESULT_OK;
      end if;

      --  so reposcver < REPO_SCHEMA_ALL

      if reposcver < int64 (Upgrade_Series'First) then
         Event.emit_error
           ("Repo " & reponame & " (schema version " & int2str (Integer (reposcver)) &
              " is too old to upgrade -- the minimum requirement is schema " &
              int2str (Integer (Upgrade_Series'First)));
         return RESULT_REPOSCHEMA;
      end if;

      if SQLite.database_was_opened_readonly (db, "main") then
         Event.emit_error
           ("Repo " & reponame & " needs schema upgrade from " & int2str (Integer (reposcver)) &
              " to " & REPO_SCHEMA_VERSION & "but it was opened as read-only");
         return RESULT_FATAL;
      end if;

      --  This repository is a candidate for upgrading
      for nextver in Upgrade_Series (reposcver + 1) .. Upgrade_Series'Last loop
         if repo_apply_upgrade (db, reponame, nextver) /= RESULT_OK then
            return RESULT_FATAL;
         end if;
      end loop;
      return RESULT_UPTODATE;
   end repo_upgrade;


   --------------------------------------------------------------------
   --  repo_apply_upgrade
   --------------------------------------------------------------------
   function repo_apply_upgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      version  : Upgrade_Series) return Action_Result
   is
      sql       : constant String := get_info (version, SQL_string);
      msg       : constant String := get_info (version, summary);
      savepoint : constant String := "SCHEMA";
      in_trans  : Boolean := False;
      rc        : Action_Result;
      errmsg    : Text;
   begin
      --  Begin Transaction
      if CommonSQL.transaction_begin (db, savepoint) then
         rc := RESULT_OK;
         in_trans := True;

         --  Apply change
         Event.emit_debug (3, "Repo mod: " & msg);
         Event.emit_debug (4, "Pkgdb: running " & SQ (sql));
         if not SQLite.exec_sql (db, sql, errmsg) then
            Event.emit_error ("sqlite: " & USS (errmsg));
            rc := RESULT_FATAL;
         end if;
      else
         rc := RESULT_FATAL;
      end if;

      --  update repo user_version
      if rc = RESULT_OK then
         rc := repo_set_version (db, version);
      end if;

      --  commit or rollback
      if in_trans then
         if rc = RESULT_OK then
            if not CommonSQL.transaction_commit (db, savepoint) then
               rc := RESULT_FATAL;
            end if;
         else
            if CommonSQL.transaction_rollback (db, savepoint) then
               null;
            end if;
         end if;
      end if;

      if rc = RESULT_OK then
         Event.emit_notice
           ("Repo " & SQ (reponame) & " upgrade schema to version " &
              int2str (Integer (version)) & ": " & msg);
      end if;
      return rc;
   end repo_apply_upgrade;


   --------------------------------------------------------------------
   --  repo_set_version
   --------------------------------------------------------------------
   function repo_set_version
     (db      : sqlite_h.sqlite3_Access;
      nextver : Upgrade_Series) return Action_Result
   is
      ver : constant Integer := Integer (nextver);
      sql : constant String :=  "PRAGMA user_version = " & int2str (ver) & ";";
      errmsg : Text;
   begin
      if SQLite.exec_sql (db, sql, errmsg) then
         return RESULT_OK;
      else
         Event.emit_error ("repo_set_version(): " & USS (errmsg));
         return RESULT_FATAL;
      end if;
   end repo_set_version;


   --------------------------------------------------------------------
   --  import_schema_2013
   --------------------------------------------------------------------
   function import_schema_2013 (db : sqlite_h.sqlite3_Access) return Action_Result
   is
      sql : constant String :=
        "CREATE TABLE packages ("
	&    "id INTEGER PRIMARY KEY,"
	&    "origin TEXT,"
	&    "name TEXT NOT NULL,"
	&    "version TEXT NOT NULL,"
	&    "comment TEXT NOT NULL,"
	&    "desc TEXT NOT NULL,"
	&    "osversion TEXT,"
	&    "arch TEXT NOT NULL,"
	&    "maintainer TEXT NOT NULL,"
	&    "www TEXT,"
	&    "prefix TEXT NOT NULL,"
	&    "pkgsize INTEGER NOT NULL,"
	&    "flatsize INTEGER NOT NULL,"
	&    "licenselogic INTEGER NOT NULL,"
	&    "cksum TEXT NOT NULL,"
	&    "path TEXT NOT NULL,"
	&    "pkg_format_version INTEGER,"
	&    "manifestdigest TEXT NULL,"
	&    "olddigest TEXT NULL,"
	&    "dep_formula TEXT NULL,"
	&    "vital INTEGER NOT NULL DEFAULT 0"
	& ");"
	& "CREATE TABLE deps ("
	&    "origin TEXT,"
	&    "name TEXT,"
	&    "version TEXT,"
	&    "package_id INTEGER REFERENCES packages(id)"
	&    "  ON DELETE CASCADE ON UPDATE CASCADE,"
	&    "UNIQUE(package_id, name)"
	& ");"
	& "CREATE TABLE categories ("
	&    "id INTEGER PRIMARY KEY, "
	&    "name TEXT NOT NULL UNIQUE "
	& ");"
	& "CREATE TABLE pkg_categories ("
	&    "package_id INTEGER REFERENCES packages(id)"
	&    "  ON DELETE CASCADE ON UPDATE CASCADE,"
	&    "category_id INTEGER REFERENCES categories(id)"
	&    "  ON DELETE RESTRICT ON UPDATE RESTRICT,"
	&    "UNIQUE(package_id, category_id)"
	& ");"
	& "CREATE TABLE licenses ("
	&     "id INTEGER PRIMARY KEY,"
	&    "name TEXT NOT NULL UNIQUE"
	& ");"
	& "CREATE TABLE pkg_licenses ("
	&    "package_id INTEGER REFERENCES packages(id)"
	&    "  ON DELETE CASCADE ON UPDATE CASCADE,"
	&    "license_id INTEGER REFERENCES licenses(id)"
	&    "  ON DELETE RESTRICT ON UPDATE RESTRICT,"
	&    "UNIQUE(package_id, license_id)"
	& ");"
	& "CREATE TABLE option ("
	& 	"option_id INTEGER PRIMARY KEY,"
	& 	"option TEXT NOT NULL UNIQUE"
	& ");"
	& "CREATE TABLE option_desc ("
	& 	"option_desc_id INTEGER PRIMARY KEY,"
	& 	"option_desc TEXT NOT NULL UNIQUE"
	& ");"
	& "CREATE TABLE pkg_option ("
	& 	"package_id INTEGER NOT NULL REFERENCES packages(id) "
	& 		"ON DELETE CASCADE ON UPDATE CASCADE,"
	& 	"option_id INTEGER NOT NULL REFERENCES option(option_id) "
	& 		"ON DELETE RESTRICT ON UPDATE CASCADE,"
	& 	"value TEXT NOT NULL,"
	& 	"PRIMARY KEY(package_id, option_id)"
	& ");"
	& "CREATE TABLE pkg_option_desc ("
	& 	"package_id INTEGER NOT NULL REFERENCES packages(id) "
	& 		"ON DELETE CASCADE ON UPDATE CASCADE,"
	& 	"option_id INTEGER NOT NULL REFERENCES option(option_id) "
	& 		"ON DELETE RESTRICT ON UPDATE CASCADE,"
	& 	"option_desc_id INTEGER NOT NULL "
	& 		"REFERENCES option_desc(option_desc_id) "
	& 		"ON DELETE RESTRICT ON UPDATE CASCADE,"
	& 	"PRIMARY KEY(package_id, option_id)"
	& ");"
	& "CREATE TABLE pkg_option_default ("
	& 	"package_id INTEGER NOT NULL REFERENCES packages(id) "
	& 		"ON DELETE CASCADE ON UPDATE CASCADE,"
	& 	"option_id INTEGER NOT NULL REFERENCES option(option_id) "
	& 		"ON DELETE RESTRICT ON UPDATE CASCADE,"
	& 	"default_value TEXT NOT NULL,"
	& 	"PRIMARY KEY(package_id, option_id)"
	& ");"
	& "CREATE TABLE shlibs ("
	&     "id INTEGER PRIMARY KEY,"
	&     "name TEXT NOT NULL UNIQUE "
	& ");"
	& "CREATE TABLE pkg_shlibs_required ("
	&     "package_id INTEGER NOT NULL REFERENCES packages(id)"
	&     "  ON DELETE CASCADE ON UPDATE CASCADE,"
	&     "shlib_id INTEGER NOT NULL REFERENCES shlibs(id)"
	&     "  ON DELETE RESTRICT ON UPDATE RESTRICT,"
	&     "UNIQUE(package_id, shlib_id)"
	& ");"
	& "CREATE TABLE pkg_shlibs_provided ("
	&     "package_id INTEGER NOT NULL REFERENCES packages(id)"
	&     "  ON DELETE CASCADE ON UPDATE CASCADE,"
	&     "shlib_id INTEGER NOT NULL REFERENCES shlibs(id)"
	&     "  ON DELETE RESTRICT ON UPDATE RESTRICT,"
	&     "UNIQUE(package_id, shlib_id)"
	& ");"
	& "CREATE TABLE annotation ("
	&     "annotation_id INTEGER PRIMARY KEY,"
	&     "annotation TEXT NOT NULL UNIQUE"
	& ");"
	& "CREATE TABLE pkg_annotation ("
	&     "package_id INTEGER REFERENCES packages(id)"
	&     " ON DELETE CASCADE ON UPDATE RESTRICT,"
	&     "tag_id INTEGER NOT NULL REFERENCES annotation(annotation_id)"
	&     " ON DELETE CASCADE ON UPDATE RESTRICT,"
	&     "value_id INTEGER NOT NULL REFERENCES annotation(annotation_id)"
	&     " ON DELETE CASCADE ON UPDATE RESTRICT,"
	&     "UNIQUE (package_id, tag_id)"
	& ");"
	& "CREATE TABLE pkg_conflicts ("
	&     "package_id INTEGER NOT NULL REFERENCES packages(id)"
	&     "  ON DELETE CASCADE ON UPDATE CASCADE,"
	&     "conflict_id INTEGER NOT NULL,"
	&     "UNIQUE(package_id, conflict_id)"
	& ");"
	& "CREATE TABLE provides("
	& "    id INTEGER PRIMARY KEY,"
	& "    provide TEXT NOT NULL"
	& ");"
	& "CREATE TABLE pkg_provides ("
	&     "package_id INTEGER NOT NULL REFERENCES packages(id)"
	&     "  ON DELETE CASCADE ON UPDATE CASCADE,"
	&     "provide_id INTEGER NOT NULL REFERENCES provides(id)"
	&     "  ON DELETE RESTRICT ON UPDATE RESTRICT,"
	&     "UNIQUE(package_id, provide_id)"
	& ");"
	& "CREATE TABLE requires("
	& "    id INTEGER PRIMARY KEY,"
	& "    require TEXT NOT NULL"
	& ");"
	& "CREATE TABLE pkg_requires ("
	& 	"package_id INTEGER NOT NULL REFERENCES packages(id)"
	& 	"  ON DELETE CASCADE ON UPDATE CASCADE,"
	& 	"require_id INTEGER NOT NULL REFERENCES requires(id)"
	& 	"  ON DELETE RESTRICT ON UPDATE RESTRICT,"
	& 	"UNIQUE(package_id, require_id)"
        & ");"
        & "CREATE TABLE pkg_search (id, name, origin);"
	& "PRAGMA user_version=2013;";
   begin
      return CommonSQL.exec (db, sql);
   end import_schema_2013;


   --------------------------------------------------------------------
   --  prstmt_text_argtypes
   --------------------------------------------------------------------
   function prstmt_text_argtypes (index : repository_stmt_index) return String is
   begin
      case index is
         when PKG          => return "TTTTTTTTTIIITTTTI";
         when DEPS         => return "TTTI";
         when CAT1         => return "T";
         when CAT2         => return "IT";
         when LIC1         => return "T";
         when LIC2         => return "IT";
         when OPT1         => return "T";
         when OPT2         => return "TTI";
         when SHLIB1       => return "T";
         when SHLIB_REQD   => return "IT";
         when SHLIB_PROV   => return "IT";
         when EXISTS       => return "T";
         when ANNOTATE1    => return "T";
         when ANNOTATE2    => return "ITT";
         when REPO_VERSION => return "T";
         when DELETE       => return "TT";
         when PROVIDE      => return "T";
         when PROVIDES     => return "IT";
         when REQUIRE      => return "T";
         when REQUIRES     => return "IT";
      end case;
   end prstmt_text_argtypes;


   --------------------------------------------------------------------
   --  prstmt_text_argtypes
   --------------------------------------------------------------------
   function prstmt_text_sql (index : repository_stmt_index) return String is
   begin
      case index is
         when PKG =>
            return
              "INSERT OR REPLACE INTO packages (origin, name, version, comment, desc, " &
              "arch, maintainer, www, prefix, pkgsize, flatsize, licenselogic, cksum, path, " &
              "manifestdigest, olddigest, vital) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, " &
              "?10, ?11, ?12, ?13, ?14, ?15, ?16, ?17)";
         when DEPS =>
            return
              "INSERT OR REPLACE INTO deps (origin, name, version, package_id) " &
              "VALUES (?1, ?2, ?3, ?4)";
         when CAT1 =>
            return
              "INSERT OR IGNORE INTO categories(name) VALUES(?1)";
         when CAT2 =>
            return
              "INSERT OR ROLLBACK INTO pkg_categories(package_id, category_id) " &
              "VALUES (?1, (SELECT id FROM categories WHERE name = ?2))";
         when LIC1 =>
            return
              "INSERT OR IGNORE INTO licenses(name) VALUES(?1)";
         when LIC2 =>
            return
              "INSERT OR ROLLBACK INTO pkg_licenses(package_id, license_id) " &
              "VALUES (?1, (SELECT id FROM licenses WHERE name = ?2))";
         when OPT1 =>
            return
              "INSERT OR IGNORE INTO option(option) VALUES (?1)";
         when OPT2 =>
            return
              "INSERT OR ROLLBACK INTO pkg_option (option_id, value, package_id) " &
              "VALUES (( SELECT option_id FROM option WHERE option = ?1), ?2, ?3)";
         when SHLIB1 =>
            return
              "INSERT OR IGNORE INTO shlibs(name) VALUES(?1)";
         when SHLIB_REQD =>
            return
              "INSERT OR IGNORE INTO pkg_shlibs_required(package_id, shlib_id) " &
              "VALUES (?1, (SELECT id FROM shlibs WHERE name = ?2))";
         when SHLIB_PROV =>
            return
              "INSERT OR IGNORE INTO pkg_shlibs_provided(package_id, shlib_id) " &
              "VALUES (?1, (SELECT id FROM shlibs WHERE name = ?2))";
         when EXISTS =>
            return
              "SELECT count(*) FROM packages WHERE cksum=?1";
         when ANNOTATE1 =>
            return
              "INSERT OR IGNORE INTO annotation(annotation) VALUES (?1)";
         when ANNOTATE2 =>
            return
              "INSERT OR ROLLBACK INTO pkg_annotation(package_id, tag_id, value_id) " &
              "VALUES (?1," &
              " (SELECT annotation_id FROM annotation WHERE annotation=?2)," &
              " (SELECT annotation_id FROM annotation WHERE annotation=?3))";
         when REPO_VERSION =>
            return
              "SELECT version FROM packages WHERE origin=?1";
         when DELETE =>
            return
              "DELETE FROM packages WHERE origin=?1;" &
              "DELETE FROM pkg_search WHERE origin=?1";
         when PROVIDE =>
            return
              "INSERT OR IGNORE INTO provides(provide) VALUES(?1)";
         when PROVIDES =>
            return
              "INSERT OR IGNORE INTO pkg_provides(package_id, provide_id) " &
              "VALUES (?1, (SELECT id FROM provides WHERE provide = ?2))";
         when REQUIRE =>
            return
              "INSERT OR IGNORE INTO requires(require) VALUES(?1)";
         when REQUIRES =>
            return
              "INSERT OR IGNORE INTO pkg_requires(package_id, require_id) " &
              "VALUES (?1, (SELECT id FROM requires WHERE require = ?2))";
      end case;
   end prstmt_text_sql;


   --------------------------------------------------------------------
   --  prstmt_finalize
   --------------------------------------------------------------------
   procedure prstmt_finalize (db : in out sqlite_h.sqlite3_Access) is
   begin
      for S in repository_stmt_index'Range loop
         SQLite.finalize_statement (prepared_statements (S));
         prepared_statements (S) := null;
      end loop;
   end prstmt_finalize;


   --------------------------------------------------------------------
   --  prstmt_initialize
   --------------------------------------------------------------------
   function prstmt_initialize (db : in out sqlite_h.sqlite3_Access) return Action_Result is
   begin
      for S in repository_stmt_index'Range loop
         Event.emit_debug (4, "Pkgdb: preparing statement " & SQ (prstmt_text_sql (S)));
         if not SQLite.prepare_sql (pDB    => db,
                                    sql    => prstmt_text_sql (S),
                                    ppStmt => prepared_statements (S)'Access)
         then
            CommonSQL.ERROR_SQLITE (db, "prstmt_initialize", prstmt_text_sql (S));
            return RESULT_FATAL;
         end if;
      end loop;
      return RESULT_OK;
   end prstmt_initialize;

end Core.Repo.Operations.Schema;
