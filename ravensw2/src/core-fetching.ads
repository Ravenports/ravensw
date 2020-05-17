--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Unix;

package Core.Fetching is

   function fetch_file_to_fd
     (reponame  : String;
      file_url  : String;
      dest_fd   : Unix.File_Descriptor;
      timestamp : Unix.T_epochtime;
      offset    : Unix.T_filesize;
      filesize  : Unix.T_filesize) return Action_Result;

end Core.Fetching;
