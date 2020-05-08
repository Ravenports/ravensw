--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Core.Strings;
private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
private with sqlite_h;

package Core.Repo is

   invalid_repo_name : exception;

   type A_repo is private;
   type A_mirror is (SRV, HTTP, NOMIRROR);
   type A_signature is (SIG_NONE, SIG_PUBKEY, SIG_FINGERPRINT);
   type A_repo_flag is (REPO_FLAGS_DEFAULT, REPO_FLAGS_LIMIT_IPV4, REPO_FLAGS_LIMIT_IPV6);
   type Init_protocol is (INIT_NONE, INIT_USE_IPV4, INIT_USE_IPV6);
   subtype A_priority is Integer;

   --  Return repo's url
   function repo_url (repo : A_repo) return String;

   --  Return repo's name
   function repo_name (repo : A_repo) return String;

   --  Return repo's public key
   function repo_pubkey (repo : A_repo) return String;

   --  Return repo's fingerprints
   function repo_fingerprints (repo : A_repo) return String;

   --  Return repo's enabled as string (yes/no)
   function repo_enabled (repo : A_repo) return String;

   --  Return repo's enabled state
   function repo_enabled (repo : A_repo) return Boolean;

   --  Return repo's mirror type as string
   function repo_mirror_type (repo : A_repo) return String;

   --  Return repo's mirror type natively
   function repo_mirror_type (repo : A_repo) return A_mirror;

   --  Return repo's signature type as string
   function repo_signature_type (repo : A_repo) return String;

   --  Return repo's signature type natively
   function repo_signature_type (repo : A_repo) return A_signature;

   --  Return repo's priority type is string
   function repo_priority_type (repo : A_repo) return String;

   --  Return repo's priority type natively
   function repo_priority_type (repo : A_repo) return A_priority;

   --  Return repo's IP protocol type is string
   function repo_ipv_type (repo : A_repo) return String;

   --  Return repo's IP protocol type natively
   function repo_ipv_type (repo : A_repo) return A_repo_flag;

   --  Return number of configured repositories that are active.
   function count_of_active_repositories return Natural;

   --  Return True if repo identified by reponame is active
   --  Returns False if reponame doesn't refer to any repo at all.
   function repository_is_active (reponame : String) return Boolean;

   --  Returns blank string if no repositories are active, otherwise it returns
   --  The name of the first one.
   function first_active_repository return String;

   --  Returns the private repository records given its reponame.  If that reponame
   --  doesn't match a known repository, the invalid_repo_name exception is thrown
   function get_repository (reponame : String) return A_repo;

   --  The repositories_order repo names are joined with unix line feeds and returned
   --  as a single string.
   function joined_priority_order return String;

private

   use Core.Strings;

   package CON renames Ada.Containers;

   --  Obsolete, but maintained for compatability
   type A_package_format is (TAR, TGZ, TBZ, TXZ, TZS);
   type A_hash_type is (HASH_UNKNOWN, HASH_SHA256, HASH_BLAKE2);
   type A_pubkey_type is (rsa);
   type A_fingerprint is
      record
         hash_type : A_hash_type;
         hash      : Text;
      end record;

   type A_checksum_type is
      (PKG_HASH_TYPE_SHA256_BASE32,
       PKG_HASH_TYPE_SHA256_HEX,
       PKG_HASH_TYPE_BLAKE2_BASE32,
       PKG_HASH_TYPE_SHA256_RAW,
       PKG_HASH_TYPE_BLAKE2_RAW,
       PKG_HASH_TYPE_BLAKE2S_BASE32,
       PKG_HASH_TYPE_BLAKE2S_RAW,
       PKG_HASH_TYPE_UNKNOWN);
   pragma Convention (C, A_checksum_type);

   type Value_EOL is mod 2**64;

   --  version 0 is an error, version 1 is the only existing version.
   type A_meta_version is new Natural range 0 .. 1;

   type A_meta_cert is
      record
         pubkey      : Text;
         pubkey_type : A_pubkey_type;
         name        : Text;
      end record;

   type DNS_srvinfo is
      record
         dns_type    : Natural;
         class       : Natural;
         ttl         : Natural;
         priority    : Natural;
         weight      : Natural;
         port        : Natural;
         finalweight : Natural;
         host        : Text;
      end record;

   type A_http_mirror is
      record
         --  url : struct url    Use the url record from fetch bind later
         urlp1 : Text;
         urlp2 : Text;
      end record;

   package A_cert_crate is new CON.Vectors
     (Element_Type => A_meta_cert,
      Index_Type   => Natural);

   package A_nvpair_crate is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Text,
      Hash            => map_hash,
      Equivalent_Keys => equivalent,
      "="             => SU."=");

   package A_DNS_srvinfo_crate is new CON.Vectors
     (Element_Type => DNS_srvinfo,
      Index_Type   => Natural);

   package A_http_mirror_crate is new CON.Vectors
     (Element_Type => A_http_mirror,
      Index_Type   => Natural);

   type Repo_metadata is
      record
         maintainer        : Text;
         source            : Text;
         source_identifier : Text;
         digests           : Text;
         digests_archive   : Text;
         manifests         : Text;
         manifests_archive : Text;
         filesite          : Text;
         filesite_archive  : Text;
         conflicts         : Text;
         conflicts_archive : Text;
         fulldb            : Text;
         fulldb_archive    : Text;
         digest_format     : A_checksum_type;
         cert              : A_cert_crate.Vector;
         packing_format    : A_package_format := TZS;
         revision          : Integer;
         end_of_life       : Value_EOL;
         version           : A_meta_version;
      end record;

   type A_repo is
      record
         name           : Text;
         url            : Text;
         pubkey         : Text;
         mirror_type    : A_mirror;
         signature_type : A_signature;
         fingerprints   : Text;
         trusted_fprint : A_fingerprint;
         revoked_fprint : A_fingerprint;
         meta           : Repo_metadata;
         enable         : Boolean;
         priority       : A_priority;
         flags          : A_repo_flag;
         env            : A_nvpair_crate.Map;
         sqlite_handle  : aliased sqlite_h.sqlite3_Access;
         --  ssh
         --  sshio

         --  can't use immutable records inside containers
         --  srv and http are used mutually exclusively
         srv            : A_DNS_srvinfo_crate.Vector;
         http           : A_http_mirror_crate.Vector;
      end record;

   package Repository_Crate is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => A_repo,
      Hash            => Strings.map_hash,
      Equivalent_Keys => Strings.equivalent);

   type Repo_Priority is
      record
         reponame : Text;
         priority : A_priority;
      end record;

   --  Order to sort A_repo
   function repo_priority_less_than (A, B : Repo_Priority) return Boolean;

   package Repos_Priority_Crate is new CON.Vectors
     (Element_Type => Repo_Priority,
      Index_Type   => Natural);

   package Priority_Sorter is new Repos_Priority_Crate.Generic_Sorting
     ("<" => repo_priority_less_than);

   repositories       : Repository_Crate.Map;
   repositories_order : Repos_Priority_Crate.Vector;

end Core.Repo;
