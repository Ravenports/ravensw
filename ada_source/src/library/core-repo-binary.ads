--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with sqlite_h;

package Core.Repo.Binary is

   type Repo_Operations_Binary is new Base_Repo_Operations with private;

   overriding
   function repo_init   (this : Repo_Operations_Binary; reponame : Text) return Boolean;

   overriding
   function repo_create (this : Repo_Operations_Binary; reponame : Text) return Boolean;

   overriding
   function repo_update (this : Repo_Operations_Binary; reponame : Text; force : Boolean)
                         return Boolean;

   overriding
   function repo_close  (this : Repo_Operations_Binary; reponame : Text; commit : Boolean)
                         return Boolean;

   overriding
   function repo_open   (this : Repo_Operations_Binary; reponame : Text; mode : mode_t)
                         return Boolean;

   overriding
   function repo_access (this : Repo_Operations_Binary; reponame : Text; mode : mode_t)
                         return Boolean;

   overriding
   function repo_ensure_loaded (this : Repo_Operations_Binary; reponame : Text; pkg1 : T_pkg)
                                return Boolean;

private
   type Repo_Operations_Binary is new Base_Repo_Operations with
      record
         variant : repo_ops_variant := Pkg.binary;
      end record;

   type binary_stmt_index is
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

      --  SQL associated with sql_prstmt_index enumeration
   function binary_stmt_text_sql (index : binary_stmt_index) return String;

   --  Argument types associated with sql_prstmt_index enumeration
   function binary_stmt_text_argtypes (index : binary_stmt_index) return String;

   binary_prepared_statements : array (binary_stmt_index) of sqlite_h.sqlite3_stmt_Access;

end Core.Repo.Binary;
