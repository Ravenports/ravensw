--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg; use Core.Pkg;
with Core.Unix;

package Core.Repo.Binary_Update is

   function pkg_repo_binary_update (reponame : Text; force : Boolean) return Pkg_Error_Type;

private

   function pkg_repo_binary_update_proceed
     (dbpath   : String;
      reponame : String;
      mtime    : in out Unix.T_epochtime;
      force    : Boolean) return Pkg_Error_Type;

   function pkg_repo_binary_init_update (reponame : String) return Boolean;

end Core.Repo.Binary_Update;
