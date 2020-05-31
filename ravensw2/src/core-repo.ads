--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Core.Pkgtypes;
with Core.Strings;
with Core.Checksum;
with Core.Unix;
with Libfetch;

private with sqlite_h;

use Core.Strings;

package Core.Repo is

   package CON renames Ada.Containers;

   invalid_repo_name : exception;

   type A_repo is private;
   type Repo_Metadata is private;
   type A_mirror is (SRV, HTTP, NOMIRROR);
   type A_signature is (SIG_NONE, SIG_PUBKEY, SIG_FINGERPRINT);
   type A_repo_flag is (REPO_FLAGS_DEFAULT, REPO_FLAGS_LIMIT_IPV4, REPO_FLAGS_LIMIT_IPV6);
   subtype A_priority is Integer;

   package Active_Repository_Name_Set is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");

   --  Return repo's url
   function repo_url (repo : A_repo) return String;

   --  Return repo's name
   function repo_name (repo : A_repo) return String;

   --  Return repo's public key
   function repo_pubkey (repo : A_repo) return String;

   --  Return repo's fingerprints
   function repo_fingerprints (repo : A_repo) return String;

   --  Return number of trusted fingerprints
   function count_of_trusted_fingerprints (repo : A_repo) return Natural;

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

   --  Return repo's priority as string
   function repo_priority (repo : A_repo) return String;

   --  Return repo's priority natively
   function repo_priority (repo : A_repo) return A_priority;

   --  Return repo's IP protocol type is string
   function repo_ipv_type (repo : A_repo) return String;

   --  Return repo's IP protocol type natively
   function repo_ipv_type (repo : A_repo) return A_repo_flag;

   --  Return number of configured repositories that are active.
   function count_of_active_repositories return Natural;

   --  Return number of known repositories (active or not)
   function total_repositories return Natural;

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
   --  as a single string.  This are in order of priority
   function joined_priority_order return String;

   --  Returns vector of active repositories in order of priority
   function ordered_active_repositories return Active_Repository_Name_Set.Vector;

   --  Returns true if repository is registered with given name
   function repository_exists (reponame : String) return Boolean;

   --  Return the metafile's digest format
   function repo_meta_digest_format (repo : A_repo) return Checksum.A_Checksum_Type;

   --  Returns the vector of environment settings
   function repo_environment (repo : A_repo) return Pkgtypes.Package_NVPairs.Map;

   --  Returns a copy of the ssh stream
   function repo_ssh (repo : A_repo) return Libfetch.Fetch_Stream;

private

   --  Obsolete, but maintained for compatability
   type A_package_format is (TAR, TGZ, TBZ, TXZ, TZS);

   type A_hash_type is (HASH_UNKNOWN, HASH_SHA256, HASH_BLAKE2);
   type A_pubkey_type is (rsa);
   type A_fingerprint is
      record
         hash_type : A_hash_type;
         hash      : Text;
      end record;

   type Value_EOL is mod 2**64;

   --  version 0 is an error, version 1 is the only existing version.
   type A_Meta_Version is new Natural range 0 .. 1;

   type Meta_Certificate is
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
         scheme   : Text;
         user     : Text;
         pwd      : Text;
         host     : Text;
         port     : Natural;
         doc      : Text;
         offset   : Integer;
         length   : Natural;
         ims_time : Unix.T_epochtime;
         netrcfd  : Unix.File_Descriptor;
      end record;

   package A_cert_crate is new CON.Vectors
     (Element_Type => Meta_Certificate,
      Index_Type   => Natural);

   package A_DNS_srvinfo_crate is new CON.Vectors
     (Element_Type => DNS_srvinfo,
      Index_Type   => Natural);

   package A_http_mirror_crate is new CON.Vectors
     (Element_Type => A_http_mirror,
      Index_Type   => Natural);

   package A_Fingerprint_crate is new CON.Vectors
     (Element_Type => A_fingerprint,
      Index_Type   => Natural);

   type SSH_IO_Info is
      record
         fd_in  : Unix.File_Descriptor;
         fd_out : Unix.File_Descriptor;
         pid    : Unix.Process_ID;
      end record;

   type Repo_Metadata is
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
         digest_format     : Checksum.A_Checksum_Type;
         cert_set          : A_cert_crate.Vector;
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
         trusted_fprint : A_Fingerprint_crate.Vector;
         revoked_fprint : A_Fingerprint_crate.Vector;
         meta           : Repo_metadata;
         enable         : Boolean;
         priority       : A_priority;
         flags          : A_repo_flag;
         env            : Pkgtypes.Package_NVPairs.Map;
         sqlite_handle  : aliased sqlite_h.sqlite3_Access;
         ssh            : Libfetch.Fetch_Stream;
         ssh_io         : SSH_IO_Info;

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

   type Priority_Identity is
      record
         reponame : Text;
         priority : A_priority;
      end record;

   --  Order to sort A_repo
   function repo_priority_less_than (A, B : Priority_Identity) return Boolean;

   package Repos_Priority_Crate is new CON.Vectors
     (Element_Type => Priority_Identity,
      Index_Type   => Natural);

   package Priority_Sorter is new Repos_Priority_Crate.Generic_Sorting
     ("<" => repo_priority_less_than);

   repositories       : Repository_Crate.Map;
   repositories_order : Repos_Priority_Crate.Vector;
   repositories_open  : Repos_Priority_Crate.Vector;

   function meta_filename (reponame : String) return String;

end Core.Repo;
