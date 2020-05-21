--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Ada.Containers.Vectors;

with Core.Unix;

package Core.Repo.Fetch is


private

   package CON renames Ada.Containers;

   function fetch_remote_tmp
     (my_repo   : A_repo;
      filename  : String;
      timestamp : Unix.T_epochtime;
      retcode   : out Action_Result) return Unix.File_Descriptor;

   function meta_extract_signature_pubkey
     (arc_fd    : Unix.File_Descriptor;
      retcode   : out Action_Result) return String;

   package Set_File_Contents is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");

   function meta_extract_signature_fingerprints
     (arc_fd : Unix.File_Descriptor;
      fingerprints : out Set_File_Contents.Vector) return Action_Result;

   function file_extension_matches (filename, extension : String) return Boolean;

   function meta_extract_to_file_descriptor
     (arc_fd    : Unix.File_Descriptor;
      target_fd : Unix.File_Descriptor;
      filename  : String) return Action_Result;

   type SC_type is (sc_unset, sc_signature, sc_certificate);

   type Signature_Certificate is
      record
         name    : Text;
         method  : SC_type;
         data    : Text;
         trusted : Boolean;
      end record;

   package Set_Signature_Certificates is new CON.Vectors
     (Element_Type => Signature_Certificate,
      Index_Type   => Natural);

   function archive_extract_archive
     (my_repo  : A_repo;
      fd       : Unix.File_Descriptor;
      filename : String;
      dest_fd  : Unix.File_Descriptor;
      cert_set : out Set_Signature_Certificates.Vector) return Action_Result;

   function parse_sigkey (encoded_sigkey : String;
                          signature      : out Signature_Certificate) return Action_Result;

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
