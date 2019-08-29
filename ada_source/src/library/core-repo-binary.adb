--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


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
   function repo_close (this : Repo_Operations_Binary; repo : T_pkg_repo; commit : Boolean)
                        return Boolean
   is
   begin
      return False;
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


end Core.Repo.Binary;
