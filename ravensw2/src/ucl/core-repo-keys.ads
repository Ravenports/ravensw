--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Unix;
with libucl;

package Core.Repo.Keys is

   function load_fingerprints (my_repo : Repo_Cursor) return Action_Result;

   function extract_public_key (metafd : Unix.File_Descriptor;
                                name   : String;
                                rc     : out Action_Result) return Text;

private

   type Cert_Validity is (trusted, revoked);

   function load_fingerprint (folder : String; filename : String) return A_fingerprint;
   function parse_fingerprint (obj : access libucl.ucl_object_t) return A_fingerprint;

   function load_fingerprints_by_type
     (my_repo  : Repo_Cursor;
      validity : Cert_Validity)
      return Action_Result;

end Core.Repo.Keys;
