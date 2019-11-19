--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;

with SQLite;
with Core.Config;
with Core.Unix;
with Core.Event;
with Core.Repo_Meta;
with Core.PkgDB_Query;
with Core.Checksum;
with Core.Iterators.Binary_sqlite;
with Core.Repo.Binary_Update;
with Core.Repo.Common;
with Core.Printf;

use Core.Repo.Common;

package body Core.Repo.Binary is

   package DIR renames Ada.Directories;
   package IBS renames Core.Iterators.Binary_sqlite;

   --------------------------------------------------------------------
   --  repo_init
   --------------------------------------------------------------------
   overriding
   function repo_init (this : Repo_Operations_Binary; reponame : Text) return Boolean
   is
      repo : T_pkg_repo renames Config.repositories.Element (reponame);
   begin
      --  TODO:
      return False;
   end repo_init;


   --------------------------------------------------------------------
   --  repo_create
   --------------------------------------------------------------------
   overriding
   function repo_create (this : Repo_Operations_Binary; reponame : Text) return Boolean
   is
   begin
      return False;
   end repo_create;


   --------------------------------------------------------------------
   --  repo_update
   --------------------------------------------------------------------
   overriding
   function repo_update (this : Repo_Operations_Binary; reponame : Text; force : Boolean)
                         return Pkg_Error_Type is
   begin
      return Repo.Binary_Update.pkg_repo_binary_update (reponame, force);
   end repo_update;


   --------------------------------------------------------------------
   --  repo_close
   --------------------------------------------------------------------
   overriding
   function repo_close (this : Repo_Operations_Binary; reponame : Text; commit : Boolean)
                        return Boolean
   is
      procedure close_database (key : Text; Element : in out T_pkg_repo);

      repo : T_pkg_repo renames Config.repositories.Element (reponame);

      procedure close_database (key : Text; Element : in out T_pkg_repo)
      is
         repository : T_pkg_repo renames Element;
      begin
         SQLite.close_database (repository.sqlite_handle);
         repository.sqlite_handle := null;
      end close_database;

   begin
      if not SQLite.db_connected (repo.sqlite_handle) then
         return True;
      end if;

      if commit then
         if not PkgDB.pkgdb_transaction_commit_sqlite (repo.sqlite_handle, "") then
            return False;
         end if;
      end if;

      for S in binary_stmt_index loop
         SQLite.finalize_statement (binary_prepared_statements (S));
      end loop;

      Config.repositories.Update_Element (Position => Config.repositories.Find (reponame),
                                          Process  => close_database'Access);
      return True;
   end repo_close;


   --------------------------------------------------------------------
   --  repo_open
   --------------------------------------------------------------------
   overriding
   function repo_open (this : Repo_Operations_Binary; reponame : Text; mode : mode_t)
                       return Boolean
   is
      procedure open_database (key : Text; Element : in out T_pkg_repo);

      dbdirfd : Unix.File_Descriptor;
      result  : Boolean := False;

      procedure open_database (key : Text; Element : in out T_pkg_repo)
      is
         repository : T_pkg_repo renames Element;

         S_reponame : constant String := USS (reponame);
         errprefix  : constant String := "Repository " & S_reponame & " load error: ";
      begin
         --  Open metafile
         declare
            fd       : Unix.File_Descriptor;
            filename : constant String := pkg_repo_binary_get_meta (S_reponame);
            dbfile   : constant String := "repo-" & S_reponame & ".sqlite";
            flags    : constant Unix.T_Open_Flags := (RDONLY => True, others => False);
            success  : Pkg_Error_Type;
            tmp      : T_pkg_repo_meta;
         begin
            fd := Unix.open_file (dirfd         => dbdirfd,
                                  relative_path => filename,
                                  flags         => flags);

            if Unix.file_connected (fd) then
               tmp := Repo_Meta.pkg_repo_meta_load (fd, success);
               if success = EPKG_OK then
                  repository.meta := tmp;
               else
                  Event.pkg_emit_errno (SUS (errprefix & "openat"),
                                        SUS ("dbdirfd, " & filename),
                                        Unix.errno);
                  if Unix.close_file (fd) then
                     null;
                  end if;
                  return;
               end if;
               if not Unix.close_file (fd) then
                  Event.pkg_emit_errno (SUS (errprefix & "close"), SUS ("meta fd"), Unix.errno);
                  return;
               end if;
            end if;

            if not Unix.relative_file_readable (dbdirfd, dbfile) then
               Event.pkg_emit_error (SUS (dbfile & " is not readable"));
               return;
            end if;

            declare
               use type sqlite_h.enum_error_types;
               opened : Boolean;
            begin
               if mode_sets_write_flag (mode) then
                  opened := SQLite.open_sqlite_database_readwrite
                    (path => dbfile,
                     ppDB => repository.sqlite_handle'Access);
               else
                  opened := SQLite.open_sqlite_database_readonly
                    (path => dbfile,
                     ppDB => repository.sqlite_handle'Access);
               end if;
               if not opened then
                  if SQLite.get_last_error_code (repository.sqlite_handle) =
                    sqlite_h.SQLITE_CORRUPT
                  then
                     Event.pkg_emit_error
                       (SUS ("Database corrupt.  Are you running on NFS?  " &
                          "If so, ensure the locking mechanism is properly set up."));
                  end if;
                  Event.pkg_emit_errno (SUS (errprefix & "open sqlite"), SUS (dbfile), Unix.errno);
                  return;
               end if;
            end;

            --  Verify database is usable
            declare
               use type SQLite.sql_int64;
               res_int64 : SQLite.sql_int64;
               sql       : constant String := "SELECT count(name) FROM sqlite_master " &
                                              "WHERE type='table' AND name='repodata'";
            begin
               if PkgDB.get_pragma (repository.sqlite_handle, sql, res_int64, False) /= EPKG_OK
               then
                  Event.pkg_emit_errno (SUS (errprefix & "pragma"), SUS (sql), Unix.errno);
                  SQLite.close_database (repository.sqlite_handle);
                  return;
               end if;

               if res_int64 /= 1 then
                  Event.pkg_emit_error
                    (SUS ("Repository " & S_reponame &
                       " contains no repodata table, database must be recreated"));
                  SQLite.close_database (repository.sqlite_handle);
                  return;
               end if;
            end;

            --  Check package site
            declare
               use type SQLite.sql_int64;
               res_int64 : SQLite.sql_int64;
               url       : constant String := Config.pkg_repo_url (repository);
               sql       : constant String := "select count(key) from repodata " &
                           "WHERE key = " & DQ ("packagesite") & " and value = " & SQ (url);
            begin
               if PkgDB.get_pragma (repository.sqlite_handle, sql, res_int64, False) /= EPKG_OK
               then
                  Event.pkg_emit_errno (SUS (errprefix & "pragma"), SUS (sql), Unix.errno);
                  SQLite.close_database (repository.sqlite_handle);
                  return;
               end if;

               if res_int64 /= 1 then
                  Event.pkg_emit_error
                    (SUS ("Repository " & S_reponame &
                       " has a wrong packagesite, database must be recreated"));
                  SQLite.close_database (repository.sqlite_handle);
                  return;
               end if;
            end;

            --  Check version
            if pkg_repo_binary_check_version (repository.sqlite_handle, S_reponame) /= EPKG_OK
            then
               Event.pkg_emit_error  (SUS ("Repository " & S_reponame &
                       " has an unsupported schema, database must be recreated"));
               SQLite.close_database (repository.sqlite_handle);
               if mode_sets_write_flag (mode) then
                  begin
                     DIR.Delete_File (dbfile);
                  exception
                     when others =>
                        Event.pkg_emit_error (SUS ("Failed to unlink " & dbfile & "!"));
                  end;
               end if;
               return;
            end if;

            --  Check digests format
            declare
               my_pkg : T_pkg_Access;
               it     : Binary_sqlite.Iterator_Binary_Sqlite :=
                 pkg_repo_binary_query (db       => repository.sqlite_handle,
                                        reponame => S_reponame,
                                        pattern  => "",
                                        match    => PkgDB.MATCH_ALL,
                                        flags    => PKGDB_IT_FLAG_ONCE);
            begin
               if invalid_iterator (Base_Iterators (it)) then
                  result := True;
                  return;
               end if;

               if it.Next (my_pkg, PKG_LOAD_FLAG_BASIC) /= EPKG_OK then
                  delete_pkg (my_pkg);
                  result := True;
                  return;
               end if;

               if IsBlank (my_pkg.digest) or else
                 not Checksum.pkg_checksum_is_valid (my_pkg.digest)
               then
                  Event.pkg_emit_error
                    (SUS ("Repository " & S_reponame &
                       " has an incompatible checksum format, database must be recreated"));
                  SQLite.close_database (repository.sqlite_handle);
               else
                  result := True;
               end if;

               delete_pkg (my_pkg);
            end;
         end;
      end open_database;
   begin
      if not SQLite.initialize_sqlite then
         return False;
      end if;
      SQLite.pkgdb_syscall_overload;

      dbdirfd := Config.pkg_get_dbdirfd;

      Config.repositories.Update_Element (Position => Config.repositories.Find (reponame),
                                          Process  => open_database'Access);
      return result;
   end repo_open;


   --------------------------------------------------------------------
   --  repo_access
   --------------------------------------------------------------------
   overriding
   function repo_access (this : Repo_Operations_Binary; reponame : Text; mode : mode_t)
                         return Pkg_Error_Type
   is
      dbdir  : constant String := Config.pkg_config_get_string (Config.conf_dbdir);
      dbname : constant String := pkg_repo_binary_get_filename (USS (reponame));
      mode1  : constant PkgDB.PkgDB_Mode_Flags := PkgDB.PkgDB_Mode_Flags (mode);
   begin
      return PkgDB.pkgdb_check_access (mode1, dbdir, dbname);
   end repo_access;


   --------------------------------------------------------------------
   --  repo_ensure_loaded
   --------------------------------------------------------------------
   overriding
   function repo_ensure_loaded
     (this     : Repo_Operations_Binary;
      reponame : Text;
      pkg_ptr  : in out T_pkg_Access;
      flags    : Load_Flags)
      return Boolean
   is
      procedure transfer_file (position : file_crate.Cursor);
      procedure transfer_directory (position : directory_crate.Cursor);

      repo : T_pkg_repo renames Config.repositories.Element (reponame);
      cached_pkg : T_pkg_Access;

      procedure transfer_file (position : file_crate.Cursor)
      is
         item : T_pkg_file renames file_crate.Element (position);
      begin
         pkg_ptr.files.Append (item);
      end transfer_file;

      procedure transfer_directory (position : directory_crate.Cursor)
      is
         item : T_pkg_dir renames directory_crate.Element (position);
         key  : Text renames directory_crate.Key (position);
      begin
         pkg_ptr.dirs.Insert (key, item);
      end transfer_directory;
   begin
      if pkg_ptr.package_type /= PKG_INSTALLED and then
        (flags and (PKG_LOAD_FLAG_FILES or PKG_LOAD_FLAG_DIRS)) /= 0 and then
        (pkg_ptr.flags and (PKG_LOAD_FLAG_FILES or PKG_LOAD_FLAG_DIRS)) /= 0
      then
         --  Try to get that information from fetched package in cache
         declare
            path : constant String := this.get_cached_name (reponame, pkg_ptr);
         begin
            if IsBlank (path) then
               return False;
            end if;

            Event.pkg_debug (1, "Binary> loading " & path);
            if pkg_open (cached_pkg, path, PKG_OPEN_TRY) /= EPKG_OK then
               delete_pkg (cached_pkg);
               return False;
            end if;
         end;

         pkg_ptr.files.Clear;
         pkg_ptr.dirs.Clear;

         cached_pkg.files.Iterate (transfer_file'Access);
         cached_pkg.dirs.Iterate (transfer_directory'Access);

         delete_pkg (cached_pkg);

         pkg_ptr.flags := pkg_ptr.flags or (PKG_LOAD_FLAG_FILES or PKG_LOAD_FLAG_DIRS);

      end if;

      return (IBS.pkgdb_ensure_loaded_sqlite (repo.sqlite_handle, pkg_ptr, flags) = EPKG_OK);
   end repo_ensure_loaded;


   --------------------------------------------------------------------
   --  binary_stmt_text_argtypes
   --------------------------------------------------------------------
   function binary_stmt_text_argtypes (index : binary_stmt_index) return String is
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
   end binary_stmt_text_argtypes;


   --------------------------------------------------------------------
   --  binary_stmt_text_sql
   --------------------------------------------------------------------
   function binary_stmt_text_sql (index : binary_stmt_index) return String is
   begin
      case index is
         when PKG =>
            return "INSERT OR REPLACE INTO packages (origin, name, version, comment, desc, " &
              "arch, maintainer, www, prefix, pkgsize, flatsize, licenselogic, cksum, path, " &
              "manifestdigest, olddigest, vital) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, " &
              "?10, ?11, ?12, ?13, ?14, ?15, ?16, ?17)";
         when DEPS =>
            return "INSERT OR REPLACE INTO deps (origin, name, version, package_id) " &
              "VALUES (?1, ?2, ?3, ?4)";
         when CAT1 =>
            return "INSERT OR IGNORE INTO categories(name) VALUES(?1)";
         when CAT2 =>
            return "INSERT OR ROLLBACK INTO pkg_categories(package_id, category_id) " &
              "VALUES (?1, (SELECT id FROM categories WHERE name = ?2))";
         when LIC1 =>
            return "INSERT OR IGNORE INTO licenses(name) VALUES(?1)";
         when LIC2 =>
            return "INSERT OR ROLLBACK INTO pkg_licenses(package_id, license_id) " &
              "VALUES (?1, (SELECT id FROM licenses WHERE name = ?2))";
         when OPT1 =>
            return "INSERT OR IGNORE INTO option(option) VALUES (?1)";
         when OPT2 =>
            return "INSERT OR ROLLBACK INTO pkg_option (option_id, value, package_id) " &
              "VALUES (( SELECT option_id FROM option WHERE option = ?1), ?2, ?3)";
         when SHLIB1 =>
            return "INSERT OR IGNORE INTO shlibs(name) VALUES(?1)";
         when SHLIB_REQD =>
            return "INSERT OR IGNORE INTO pkg_shlibs_required(package_id, shlib_id) " &
              "VALUES (?1, (SELECT id FROM shlibs WHERE name = ?2))";
         when SHLIB_PROV =>
            return "INSERT OR IGNORE INTO pkg_shlibs_provided(package_id, shlib_id) " &
              "VALUES (?1, (SELECT id FROM shlibs WHERE name = ?2))";
         when EXISTS =>
            return "SELECT count(*) FROM packages WHERE cksum=?1";
         when ANNOTATE1 =>
            return "INSERT OR IGNORE INTO annotation(annotation) VALUES (?1)";
         when ANNOTATE2 =>
            return "INSERT OR ROLLBACK INTO pkg_annotation(package_id, tag_id, value_id) " &
              "VALUES (?1," &
              " (SELECT annotation_id FROM annotation WHERE annotation=?2)," &
              " (SELECT annotation_id FROM annotation WHERE annotation=?3))";
         when REPO_VERSION =>
            return "SELECT version FROM packages WHERE origin=?1";
         when DELETE =>
            return "DELETE FROM packages WHERE origin=?1;" &
              "DELETE FROM pkg_search WHERE origin=?1";
         when PROVIDE =>
            return "INSERT OR IGNORE INTO provides(provide) VALUES(?1)";
         when PROVIDES =>
            return "INSERT OR IGNORE INTO pkg_provides(package_id, provide_id) " &
              "VALUES (?1, (SELECT id FROM provides WHERE provide = ?2))";
         when REQUIRE =>
            return "INSERT OR IGNORE INTO requires(require) VALUES(?1)";
         when REQUIRES =>
            return "INSERT OR IGNORE INTO pkg_requires(package_id, require_id) " &
              "VALUES (?1, (SELECT id FROM requires WHERE require = ?2))";
      end case;
   end binary_stmt_text_sql;


   --------------------------------------------------------------------
   --  pkg_repo_binary_get_user_version
   --------------------------------------------------------------------
   function pkg_repo_binary_get_user_version
     (db : sqlite_h.sqlite3_Access;
      reposcver : out Integer) return Boolean
   is
      sql    : constant String := "PRAGMA user_version";
      stmt   : aliased sqlite_h.sqlite3_stmt_Access;
      result : Boolean;
      invalid : constant Integer := -1;
   begin
      if not SQLite.prepare_sql (db, sql, stmt'Access) then
         PkgDB.ERROR_SQLITE (db, "pkg_repo_binary_get_user_version", sql);
         reposcver := invalid;
         return False;
      end if;
      if SQLite.step_through_statement (stmt) then
         result := True;
         reposcver := Integer (SQLite.retrieve_integer (stmt, 0));
      else
         reposcver := invalid;
         result := False;
      end if;

      SQLite.finalize_statement (stmt);
      return result;
   end pkg_repo_binary_get_user_version;


   --------------------------------------------------------------------
   --  pkg_repo_binary_check_version
   --------------------------------------------------------------------
   function pkg_repo_binary_check_version
     (db   : sqlite_h.sqlite3_Access; reponame : String) return Pkg_Error_Type
   is
      reposcver : Integer;
      repomajor : Integer;
      rc : Pkg_Error_Type;
   begin
      if not pkg_repo_binary_get_user_version (db, reposcver) then
         return EPKG_FATAL;
      end if;

      --  If the local pkgng uses a repo schema behind that used to
      --  create the repo, we may still be able use it for reading
      --  (ie pkg install), but pkg repo can't do an incremental
      --  update unless the actual schema matches the compiled in
      --  schema version.
      --
      --  Use a major - minor version schema: as the user_version
      --  PRAGMA takes an integer version, encode this as MAJOR *
      --  1000 + MINOR.
      --
      --  So long as the major versions are the same, the local pkgng
      --  should be compatible with any repo created by a more recent
      --  pkgng, although it may need some modification of the repo
      --  schema

      repomajor := reposcver / 1000;

      if repomajor < REPO_SCHEMA_MAJOR then
         Event.pkg_emit_error
           (SUS ("Repo " & reponame & " (schema version " & int2str (reposcver) &
              " is too old -- the minimum requirement is schema " &
              int2str (REPO_SCHEMA_MAJOR * 1000)));
         return EPKG_REPOSCHEMA;
      end if;

      if repomajor > REPO_SCHEMA_MAJOR then
         Event.pkg_emit_error
           (SUS ("Repo " & reponame & " (schema version " & int2str (reposcver) &
              " is too new -- the maximum requirement is schema " &
              int2str (((REPO_SCHEMA_MAJOR + 1) * 1000) - 1)));
         return EPKG_REPOSCHEMA;
      end if;

      rc := EPKG_OK;

      declare
         RSV : constant Integer := Integer'Value (REPO_SCHEMA_VERSION);
      begin
         if reposcver < RSV then
            if SQLite.database_was_opened_readonly (db, "main") then
               Event.pkg_emit_error
                 (SUS ("Repo " & reponame & " needs schema upgrade from " & int2str (reposcver) &
                    " to " & REPO_SCHEMA_VERSION & "but it was opened as read-only"));
               rc := EPKG_FATAL;
            else
               rc := pkg_repo_binary_upgrade (db, reponame, reposcver);
            end if;
         elsif reposcver > RSV then
            if SQLite.database_was_opened_readonly (db, "main") then
               Event.pkg_emit_error
                 (SUS ("Repo " & reponame & " needs schema downgrade from " &
                    int2str (reposcver) & " to " & REPO_SCHEMA_VERSION &
                    "but it was opened as read-only"));
               rc := EPKG_FATAL;
            else
               rc := pkg_repo_binary_downgrade (db, reponame, reposcver);
            end if;
         end if;
      end;

      return rc;
   end pkg_repo_binary_check_version;


   --------------------------------------------------------------------
   --  next_version
   --------------------------------------------------------------------
   function next_version (given_version : upgrade_range) return Positive is
   begin
      case given_version is
         when 2013 => return 2014;
      end case;
   end next_version;


   --------------------------------------------------------------------
   --  upgrade_message
   --------------------------------------------------------------------
   function upgrade_message (given_version : upgrade_range) return String is
   begin
      case given_version is
         when 2013 => return "Remove full text search feature";
      end case;
   end upgrade_message;


   --------------------------------------------------------------------
   --  upgrade_sql
   --------------------------------------------------------------------
   function upgrade_sql (given_version : upgrade_range) return String is
   begin
      case given_version is
         when 2013 => return "DROP TABLE pkg_search;";
      end case;
   end upgrade_sql;


   --------------------------------------------------------------------
   --  previous_version
   --------------------------------------------------------------------
   function previous_version (given_version : downgrade_range) return Positive is
   begin
      case given_version is
         when 2013 => return 2012;
      end case;
   end previous_version;


   --------------------------------------------------------------------
   --  downgrade_message
   --------------------------------------------------------------------
   function downgrade_message (given_version : downgrade_range) return String is
   begin
      case given_version is
         when 2013 => return "Drop vital column on packages";
      end case;
   end downgrade_message;


   --------------------------------------------------------------------
   --  downgrade_sql
   --------------------------------------------------------------------
   function downgrade_sql (given_version : downgrade_range) return String is
   begin
      case given_version is
         when 2013 => return
              "ALTER TABLE packages RENAME TO packages_old;" &
              "CREATE TABLE packages (" &
                "id INTEGER PRIMARY KEY," &
                "origin TEXT UNIQUE," &
                "name TEXT NOT NULL," &
                "version TEXT NOT NULL," &
                "comment TEXT NOT NULL," &
                "desc TEXT NOT NULL," &
                "osversion TEXT," &
                "arch TEXT NOT NULL," &
                "maintainer TEXT NOT NULL," &
                "www TEXT," &
                "prefix TEXT NOT NULL," &
                "pkgsize INTEGER NOT NULL," &
                "flatsize INTEGER NOT NULL," &
                "licenselogic INTEGER NOT NULL," &
                "cksum TEXT NOT NULL," &
                "path TEXT NOT NULL," &
                "pkg_format_version INTEGER," &
                "manifestdigest TEXT NULL," &
                "olddigest TEXT NULL," &
                "dep_formula TEXT NULL," &
              ");" &
              "INSERT INTO packages (id, origin, name, version, comment, desc," &
                "osversion, arch, maintainer, www, prefix, pkgsize, flatsize," &
                "licenselogic, cksum, path, pkg_format_version, manifestdigest, olddigest) " &
                "SELECT id, origin, name, version, comment, desc," &
                "osversion, arch, maintainer, www, prefix, pkgsize, flatsize," &
                "licenselogic, cksum, path, pkg_format_version, manifestdigest, olddigest FROM " &
                "packages_old;" &
              "DROP TABLE packages_old;" &
              "CREATE INDEX packages_origin ON packages(origin COLLATE NOCASE);" &
              "CREATE INDEX packages_name ON packages(name COLLATE NOCASE);" &
              "CREATE INDEX packages_uid_nocase ON packages" &
                "(name COLLATE NOCASE, origin COLLATE NOCASE);" &
              "CREATE INDEX packages_version_nocase ON packages(name COLLATE NOCASE, version);" &
              "CREATE INDEX packages_uid ON packages(name, origin);" &
              "CREATE INDEX packages_version ON packages(name, version);" &
              "CREATE UNIQUE INDEX packages_digest ON packages(manifestdigest);";
      end case;
   end downgrade_sql;


   --------------------------------------------------------------------
   --  pkg_repo_binary_upgrade
   --------------------------------------------------------------------
   function pkg_repo_binary_upgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      current_version : Integer) return Pkg_Error_Type
   is
      RSV : constant Integer := Integer'Value (REPO_SCHEMA_VERSION);
      rc  : Pkg_Error_Type := EPKG_OK;
   begin
      --  Only run if reposcver < RSV (bank on it)
      if current_version < Integer (upgrade_range'First)
      then
         Event.pkg_emit_error
           (SUS ("Repository " & reponame & " (version " & int2str (current_version) &
              ") is too old to upgrade. The oldest supported version is " &
              int2str (Integer (upgrade_range'First))));
         return EPKG_FATAL;
      end if;
      for ver in current_version .. RSV - 1 loop
         rc := pkg_repo_binary_apply_change (db              => db,
                                             reponame        => reponame,
                                             current_version => ver,
                                             upgrade         => True);
         exit when rc /= EPKG_OK;
      end loop;

      return rc;
   end pkg_repo_binary_upgrade;


   --------------------------------------------------------------------
   --  pkg_repo_binary_downgrade
   --------------------------------------------------------------------
   function pkg_repo_binary_downgrade
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      current_version : Integer) return Pkg_Error_Type
   is
      RSV : constant Integer := Integer'Value (REPO_SCHEMA_VERSION);
      rc  : Pkg_Error_Type := EPKG_OK;
   begin
      --  Only run if reposcver > RSV (bank on it)
      if current_version > Integer (downgrade_range'Last)
      then
         Event.pkg_emit_error
           (SUS ("Repository " & reponame & " (version " & int2str (current_version) &
              ") is too new to downgrade. The latest supported version is " &
              int2str (Integer (downgrade_range'Last))));
         return EPKG_FATAL;
      end if;
      for ver in reverse RSV + 1 .. current_version loop
         rc := pkg_repo_binary_apply_change (db              => db,
                                             reponame        => reponame,
                                             current_version => ver,
                                             upgrade         => False);
         exit when rc /= EPKG_OK;
      end loop;

      return rc;
   end pkg_repo_binary_downgrade;


   --------------------------------------------------------------------
   --  pkg_repo_binary_set_version
   --------------------------------------------------------------------
   function pkg_repo_binary_set_version
     (db : sqlite_h.sqlite3_Access;
      reposcver : Integer) return Pkg_Error_Type
   is
      sql : constant String :=  "PRAGMA user_version = " & int2str (reposcver) & ";";
      errmsg : Text;
   begin
      if SQLite.exec_sql (db, sql, errmsg) then
         return EPKG_OK;
      else
         Event.pkg_emit_error (SUS ("pkg_repo_binary_set_version(): " & USS (errmsg)));
         return EPKG_FATAL;
      end if;
   end pkg_repo_binary_set_version;


   --------------------------------------------------------------------
   --  pkg_repo_binary_apply_change
   --------------------------------------------------------------------
   function pkg_repo_binary_apply_change
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      current_version : Integer;
      upgrade : Boolean) return Pkg_Error_Type
   is
      use type Pkg_Error_Type;

      nextver  : Integer;
      nextsql  : Text;
      nextmsg  : Text;
      action   : Text;
      in_trans : Boolean := False;
      rc       : Pkg_Error_Type;
   begin
      if upgrade then
         nextver := next_version (upgrade_range (current_version));
         nextsql := SUS (upgrade_sql (upgrade_range (current_version)));
         nextmsg := SUS (upgrade_message (upgrade_range (current_version)));
         action  := SUS ("upgrade");
      else
         nextver := previous_version (downgrade_range (current_version));
         nextsql := SUS (downgrade_sql (downgrade_range (current_version)));
         nextmsg := SUS (downgrade_message (downgrade_range (current_version)));
         action  := SUS ("downgrade");
      end if;

      declare
         sql       : constant String := USS (nextsql);
         msg       : constant String := USS (nextmsg);
         savepoint : constant String := "SCHEMA";
         errmsg    : Text;
      begin

         --  Begin Transaction
         if PkgDB.pkgdb_transaction_begin_sqlite (db, savepoint) then
            rc := EPKG_OK;
            in_trans := True;

            --  Apply change
            Event.pkg_debug (3, "Repo mod: " & msg);
            Event.pkg_debug (4, "Pkgdb: running '" & sql & "'");
            if not SQLite.exec_sql (db, sql, errmsg) then
               Event.pkg_emit_error (SUS ("sqlite: " & USS (errmsg)));
               rc := EPKG_FATAL;
            end if;
         end if;

         --  update repo user_version
         if rc = EPKG_OK then
            rc := pkg_repo_binary_set_version (db, nextver);
         end if;

         --  commit or rollback
         if in_trans then
            if rc = EPKG_OK then
               if not PkgDB.pkgdb_transaction_commit_sqlite (db, savepoint) then
                  rc := EPKG_FATAL;
               end if;
            else
               if PkgDB.pkgdb_transaction_rollback_sqlite (db, savepoint) then
                  null;
               end if;
            end if;
         end if;

         if rc = EPKG_OK then
            Event.pkg_emit_notice
              (SUS ("Repo '" & reponame & "' " & USS (action) & " schema " &
                 int2str (current_version) & " to " & int2str (nextver) & ": " & msg));
         end if;
      end;

      return rc;
   end pkg_repo_binary_apply_change;


   --------------------------------------------------------------------
   --  pkg_repo_binary_query
   --------------------------------------------------------------------
   function pkg_repo_binary_query
     (db       : sqlite_h.sqlite3_Access;
      reponame : String;
      pattern  : String;
      match    : PkgDB.T_match;
      flags    : Iterator_Flags) return Binary_sqlite.Iterator_Binary_Sqlite
   is
      use type PkgDB.T_match;

      stmt : aliased sqlite_h.sqlite3_stmt_Access;
      comp : String := PkgDB_Query.pkgdb_get_pattern_query (pattern, match);
      sql  : String :=
        "SELECT id, origin, name, name as uniqueid, version, comment, prefix, desc, arch, " &
        "maintainer, www, licenselogic, flatsize, pkgsize, cksum, manifestdigest, " &
        "path AS repopath, '" & reponame & "' AS dbname" &
        " FROM packages AS p" &
        comp &
        " ORDER BY name;";
   begin
      if match /= PkgDB.MATCH_ALL and then IsBlank (pattern) then
         return Binary_sqlite.create_invalid_iterator;
      end if;

      if IsBlank (pattern) then
         Event.pkg_debug (4, "Pkgdb: running '" & sql & "' query for all");
      else
         Event.pkg_debug (4, "Pkgdb: running '" & sql & "' query for " & pattern);
      end if;

      if not SQLite.prepare_sql (db, sql, stmt'Access) then
         PkgDB.ERROR_SQLITE (db, "pkg_repo_binary_query", sql);
         return Binary_sqlite.create_invalid_iterator;
      end if;

      case match is
         when PkgDB.MATCH_ALL | PkgDB.MATCH_CONDITION => null;
         when PkgDB.MATCH_EXACT | PkgDB.MATCH_GLOB | PkgDB.MATCH_REGEX =>
            SQLite.bind_string (stmt, 1, pattern);
      end case;

      return Binary_sqlite.create (db           => db,
                                   stmt         => stmt,
                                   package_type => PKG_REMOTE,
                                   flags        => PKGDB_IT_FLAG_ONCE);
   end pkg_repo_binary_query;


   --------------------------------------------------------------------
   --  get_cached_name
   --------------------------------------------------------------------
   function get_cached_name
     (this     : Repo_Operations_Binary;
      reponame : Text;
      pkg_ptr  : in out T_pkg_Access)
      return String
   is
      repo : T_pkg_repo renames Config.repositories.Element (reponame);
      packagesite : constant String := USS (repo.url);
      repopath    : constant String := USS (pkg_ptr.repopath);
      cachedir    : constant String := USS (context.cachedir) & "/";
   begin
      if leads (packagesite, "file:/") then
         return packagesite (packagesite'First + 6 .. packagesite'Last) & "/" & repopath;
      end if;

      if contains (repopath, ".") then
         --  The real naming scheme:
         --  <cachedir>/<name>-<version>-<checksum>.tzst
         --  %S/%n-%v-%z%S
         declare
            function get_extension return String;
            function get_extension return String
            is
               numdot : constant Natural := count_char (repopath, '.');
            begin
               return specific_field (repopath, numdot + 1);
            end get_extension;

            dest : constant String := cachedir &
              Printf.format_attribute (pkg_ptr.all, Printf.PKG_NAME) & '-' &
              Printf.format_attribute (pkg_ptr.all, Printf.PKG_VERSION) & '-' &
              Printf.format_attribute (pkg_ptr.all, Printf.PKG_CHECKSUM) & '.' & get_extension;

            sb : aliased Unix.struct_stat;
         begin
            if Unix.stat_ok (dest, sb'Unchecked_Access) and then
              T_pkg_size (Unix.get_file_size (sb'Unchecked_Access)) = pkg_ptr.pkgsize
            then
               return dest;
            else
               return "";
            end if;
         end;
      else
         --  %S/%n-%v-%z
         return cachedir &
           Printf.format_attribute (pkg_ptr.all, Printf.PKG_NAME) & '-' &
           Printf.format_attribute (pkg_ptr.all, Printf.PKG_VERSION) & '-' &
           Printf.format_attribute (pkg_ptr.all, Printf.PKG_CHECKSUM);
      end if;

   end get_cached_name;


end Core.Repo.Binary;
