--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Repo is

   --------------------------------------------------------------------
   --  repo_url
   --------------------------------------------------------------------
   function repo_url (repo : A_repo) return String is
   begin
      return USS (repo.url);
   end repo_url;


   --------------------------------------------------------------------
   --  repo_name
   --------------------------------------------------------------------
   function repo_name (repo : A_repo) return String is
   begin
      return USS (repo.name);
   end repo_name;


   --------------------------------------------------------------------
   --  repo_pubkey
   --------------------------------------------------------------------
   function repo_pubkey (repo : A_repo) return String is
   begin
      return USS (repo.pubkey);
   end repo_pubkey;


   --------------------------------------------------------------------
   --  repo_fingerprints
   --------------------------------------------------------------------
   function repo_fingerprints (repo : A_repo) return String is
   begin
      return USS (repo.fingerprints);
   end repo_fingerprints;


   --------------------------------------------------------------------
   --  repo_enabled #1
   --------------------------------------------------------------------
   function repo_enabled (repo : A_repo) return String is
   begin
      case repo.enable is
         when False => return "no";
         when True  => return "yes";
      end case;
   end repo_enabled;


   --------------------------------------------------------------------
   --  repo_enabled #2
   --------------------------------------------------------------------
   function repo_enabled (repo : A_repo) return Boolean is
   begin
      return repo.enable;
   end repo_enabled;


   --------------------------------------------------------------------
   --  repo_mirror_type #1
   --------------------------------------------------------------------
   function repo_mirror_type (repo : A_repo) return String is
   begin
      case repo.mirror_type is
         when SRV      => return "SRV";
         when HTTP     => return "HTTP";
         when NOMIRROR => return "NONE";
      end case;
   end repo_mirror_type;


   --------------------------------------------------------------------
   --  repo_mirror_type #2
   --------------------------------------------------------------------
   function repo_mirror_type (repo : A_repo) return A_mirror is
   begin
      return repo.mirror_type;
   end repo_mirror_type;


   --------------------------------------------------------------------
   --  repo_signature_type #1
   --------------------------------------------------------------------
   function repo_signature_type (repo : A_repo) return String is
   begin
      case repo.signature_type is
         when SIG_PUBKEY      => return "PUBKEY";
         when SIG_FINGERPRINT => return "FINGERPRINTS";
         when SIG_NONE        => return "NONE";
      end case;
   end repo_signature_type;


   --------------------------------------------------------------------
   --  repo_signature_type #2
   --------------------------------------------------------------------
   function repo_signature_type (repo : A_repo) return A_signature is
   begin
      return repo.signature_type;
   end repo_signature_type;


   --------------------------------------------------------------------
   --  repo_priority_type #1
   --------------------------------------------------------------------
   function repo_priority_type (repo : A_repo) return String is
   begin
      return int2str (Integer (repo.priority));
   end repo_priority_type;


   --------------------------------------------------------------------
   --  repo_priority_type #2
   --------------------------------------------------------------------
   function repo_priority_type (repo : A_repo) return A_priority is
   begin
      return repo.priority;
   end repo_priority_type;


   --------------------------------------------------------------------
   --  repo_ipv_type #1
   --------------------------------------------------------------------
   function repo_ipv_type (repo : A_repo) return String is
   begin
      case repo.flags is
         when REPO_FLAGS_LIMIT_IPV4 => return "4";
         when REPO_FLAGS_LIMIT_IPV6 => return "6";
         when REPO_FLAGS_DEFAULT    => return "0";
      end case;
   end repo_ipv_type;


   --------------------------------------------------------------------
   --  repo_ipv_type #2
   --------------------------------------------------------------------
   function repo_ipv_type (repo : A_repo) return A_repo_flag is
   begin
      return repo.flags;
   end repo_ipv_type;

end Core.Repo;
