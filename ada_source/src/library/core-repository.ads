--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;  use Core.Pkg;
with Core.Unix;

package Core.Repository is

   function pkg_repo_fetch_meta (repo : in out T_pkg_repo; timestamp : T_pkg_timestamp)
                                 return Pkg_Error_Type;
private

   function pkg_repo_fetch_remote_tmp
     (repo      : T_pkg_repo;
      filename  : String;
      timestamp : T_pkg_timestamp;
      rc        : out Pkg_Error_Type) return Unix.File_Descriptor;


end Core.Repository;
