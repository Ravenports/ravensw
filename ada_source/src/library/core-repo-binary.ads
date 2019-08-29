--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


package Core.Repo.Binary is

   type Repo_Operations_Binary is new Base_Repo_Operations with private;

   overriding
   function repo_init   (this : Repo_Operations_Binary; repo : T_pkg_repo) return Boolean;

   overriding
   function repo_create (this : Repo_Operations_Binary; repo : T_pkg_repo) return Boolean;

   overriding
   function repo_update (this : Repo_Operations_Binary; repo : T_pkg_repo; force : Boolean)
                         return Boolean;

   overriding
   function repo_close  (this : Repo_Operations_Binary; repo : T_pkg_repo; commit : Boolean)
                         return Boolean;

   overriding
   function repo_open   (this : Repo_Operations_Binary; repo : T_pkg_repo; mode : mode_t)
                         return Boolean;

   overriding
   function repo_access (this : Repo_Operations_Binary; repo : T_pkg_repo; mode : mode_t)
                         return Boolean;

   overriding
   function repo_ensure_loaded (this : Repo_Operations_Binary; repo : T_pkg_repo; pkg1 : T_pkg)
                                return Boolean;

private
   type Repo_Operations_Binary is new Base_Repo_Operations with
      record
         variant : repo_ops_variant := Pkg.binary;
      end record;

end Core.Repo.Binary;
