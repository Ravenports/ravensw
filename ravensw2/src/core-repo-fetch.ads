--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Unix;

package Core.Repo.Fetch is


private

   function fetch_remote_tmp
     (my_repo   : A_repo;
      filename  : String;
      timestamp : Unix.T_epochtime;
      retcode   : out Action_Result) return Unix.File_Descriptor;

   function meta_extract_signature_pubkey
     (arc_fd    : Unix.File_Descriptor;
      retcode   : out Action_Result) return String;

   function meta_extract_signature_fingerprints
     (arc_fd    : Unix.File_Descriptor;
      retcode   : out Action_Result) return String;

   function file_extension_matches (filename, extension : String) return Boolean;

   function meta_extract_to_file_descriptor
     (arc_fd    : Unix.File_Descriptor;
      target_fd : Unix.File_Descriptor;
      filename  : String) return Action_Result;

   type Signature_Certificate is
      record
         name    : Text;
         sig     : Text;
         cert    : Text;
         trusted : Boolean;
      end record;

--     function archive_extract_archive
--       (my_repo   : A_repo;
--        fd        : Unix.File_Descriptor;
--        filename  : String;
--        dest_fd   : Unix.File_Descriptor;
--        signature : out Signature_Certificate) return Action_Result;
--
--     function archive_extract_check_archive
--       (my_repo   : A_repo;
--        fd        : Unix.File_Descriptor;
--        filename  : String;
--        dest_fd   : Unix.File_Descriptor) return Action_Result;
--
--     function fetch_meta
--       (my_repo   : A_repo;
--        timestamp : Unix.T_epochtime) return Action_Result;


end Core.Repo.Fetch;
