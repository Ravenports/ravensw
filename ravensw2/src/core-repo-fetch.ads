--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Unix;

package Core.Repo.Fetch is

--     function fetch_meta
--       (reponame : String;
--        timestamp : Pkgtypes.Package_Timestamp) return Action_Result;

private

   function fetch_remote_tmp
     (my_repo   : A_repo;
      filename  : String;
      extension : String;
      timestamp : Unix.T_epochtime;
      rc        : out Action_Result) return Unix.File_Descriptor;

end Core.Repo.Fetch;
