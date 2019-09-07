--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Repo.Common is


   --------------------------------------------------------------------
   --  pkg_repo_binary_get_filename
   --------------------------------------------------------------------
   function pkg_repo_binary_get_filename (reponame : String) return String
   is
   begin
      return reponame & ".sqlite";
   end pkg_repo_binary_get_filename;


   --------------------------------------------------------------------
   --  pkg_repo_binary_get_meta
   --------------------------------------------------------------------
   function pkg_repo_binary_get_meta (reponame : String) return String
   is
   begin
      return reponame & ".meta";
   end pkg_repo_binary_get_meta;

end Core.Repo.Common;
