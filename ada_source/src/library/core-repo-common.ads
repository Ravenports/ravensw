--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Repo.Common is

   --  Returns <reponame>.sqlite
   function pkg_repo_binary_get_filename (reponame : String) return String;

   --  Returns <reponame>.meta
   function pkg_repo_binary_get_meta (reponame : String) return String;

end Core.Repo.Common;
