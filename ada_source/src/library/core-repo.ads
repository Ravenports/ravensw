--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;  use Core.Pkg;
with Core.Strings; use Core.Strings;

package Core.Repo is

   ACCESS_F_OK : constant mode_t := 0;
   ACCESS_X_OK : constant mode_t := 1;
   ACCESS_W_OK : constant mode_t := 2;
   ACCESS_R_OK : constant mode_t := 4;

   type supported_ops is (init, create, update, close, open, raccess, ensure_loaded);

   type Base_Repo_Operations is abstract tagged null record;
   type Repo_Ops_Access is access all Base_Repo_Operations'Class;

   function repo_init   (this : Base_Repo_Operations; reponame : Text)
                         return Boolean is abstract;
   function repo_create (this : Base_Repo_Operations; reponame : Text)
                         return Boolean is abstract;
   function repo_update (this : Base_Repo_Operations; reponame : Text; force : Boolean)
                         return Boolean is abstract;
   function repo_close  (this : Base_Repo_Operations; reponame : Text; commit : Boolean)
                         return Boolean is abstract;
   function repo_open   (this : Base_Repo_Operations; reponame : Text; mode : mode_t)
                         return Boolean is abstract;
   function repo_access (this : Base_Repo_Operations; reponame : Text; mode : mode_t)
                         return Pkg_Error_Type is abstract;

   --  TODO: add flags to repo_ensure loaded
   function repo_ensure_loaded (this : Base_Repo_Operations; reponame : Text; pkg1 : T_pkg)
                                return Boolean is abstract;

   --  .query = pkg_repo_binary_query,
   --  .shlib_provided = pkg_repo_binary_shlib_provide,
   --  .shlib_required = pkg_repo_binary_shlib_require,
   --  .provided = pkg_repo_binary_provide,
   --  .required = pkg_repo_binary_require,
   --  .search = pkg_repo_binary_search,
   --  .fetch_pkg = pkg_repo_binary_fetch,
   --  .mirror_pkg = pkg_repo_binary_mirror,
   --  .get_cached_name = pkg_repo_binary_get_cached_name,
   --  .stat = pkg_repo_binary_stat

   function mode_sets_write_flag (mode : mode_t) return Boolean;

end Core.Repo;
