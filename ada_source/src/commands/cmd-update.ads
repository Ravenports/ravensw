--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;     use Core.Pkg;

package Cmd.Update is

   --  Fetch repository catalogs
   function pkgcli_update (force  : Boolean;
                           strict : Boolean;
                           quiet  : Boolean;
                           reponame : String) return Pkg_Error_Type;

end Cmd.Update;
