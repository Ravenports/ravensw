--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with SQLite;

package body Core.Repo.Binary is


   --------------------------------------------------------------------
   --  repo_init
   --------------------------------------------------------------------
   overriding
   function repo_init (this : Repo_Operations_Binary; repo : T_pkg_repo) return Boolean
   is
   begin
      return False;
   end repo_init;


   --------------------------------------------------------------------
   --  repo_create
   --------------------------------------------------------------------
   overriding
   function repo_create (this : Repo_Operations_Binary; repo : T_pkg_repo) return Boolean
   is
   begin
      return False;
   end repo_create;


   --------------------------------------------------------------------
   --  repo_update
   --------------------------------------------------------------------
   overriding
   function repo_update (this : Repo_Operations_Binary; repo : T_pkg_repo; force : Boolean)
                         return Boolean
   is
   begin
      return False;
   end repo_update;


   --------------------------------------------------------------------
   --  repo_close
   --------------------------------------------------------------------
   overriding
   function repo_close (this : Repo_Operations_Binary; repo : in out T_pkg_repo; commit : Boolean)
                        return Boolean
   is
   begin
      if not SQLite.db_connected (repo.sqlite_handle) then
         return True;
      end if;

      if commit then
         null;
      end if;

      for S in binary_stmt_index loop
         SQLite.finalize_statement (binary_prepared_statements (S));
      end loop;
      SQLite.close_database (repo.sqlite_handle);

      repo.sqlite_handle := null;
      return True;
   end repo_close;


   --------------------------------------------------------------------
   --  repo_open
   --------------------------------------------------------------------
   overriding
   function repo_open (this : Repo_Operations_Binary; repo : T_pkg_repo; mode : mode_t)
                       return Boolean
   is
   begin
      return False;
   end repo_open;


   --------------------------------------------------------------------
   --  repo_access
   --------------------------------------------------------------------
   overriding
   function repo_access (this : Repo_Operations_Binary; repo : T_pkg_repo; mode : mode_t)
                         return Boolean
   is
   begin
      return False;
   end repo_access;


   --------------------------------------------------------------------
   --  repo_ensure_loaded
   --------------------------------------------------------------------
   overriding
   function repo_ensure_loaded (this : Repo_Operations_Binary; repo : T_pkg_repo; pkg1 : T_pkg)
                                return Boolean
   is
   begin
      return False;
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


end Core.Repo.Binary;
