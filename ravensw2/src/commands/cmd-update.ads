--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


package Cmd.Update is

   --  Fetch repository catalogs
   function pkgcli_update (force    : Boolean;
                           strict   : Boolean;
                           quiet    : Boolean;
                           reponame : String) return Action_Result;

end Cmd.Update;
