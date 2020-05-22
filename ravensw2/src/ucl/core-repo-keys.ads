--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with libucl;

package Core.Repo.Keys is

   function load_fingerprints (reponame : String) return Action_Result;

private

   type Cert_Validity is (trusted, revoked);

   function load_fingerprint (folder : String; filename : String) return A_fingerprint;
   function parse_fingerprint (obj : access libucl.ucl_object_t) return A_fingerprint;

   function load_fingerprints_by_type
     (my_repo  : in out A_repo;
      validity : Cert_Validity)
      return Action_Result;

end Core.Repo.Keys;
