--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Core.Unix;
with Core.Strings;
with sqlite_h;

package Core.Pkg is

   package CON renames Ada.Containers;

   type Pkg_Error_Type is
     (
      EPKG_OK,
      EPKG_END,        --  No more items available (end of the loop)
      EPKG_WARN,       --  The function encountered a non-fatal error
      EPKG_FATAL,      --  The function encountered a fatal error
      EPKG_REQUIRED,   --  Can not delete the package because it is required by another package
      EPKG_INSTALLED,  --  Can not install the package because it is already installed.
      EPKG_DEPENDENCY, --  Can not install the package because some dependencies are unresolved
      EPKG_LOCKED,     --  Can not operate on package because it is locked
      EPKG_ENODB,      --  Can not create local database or database non-existent
      EPKG_UPTODATE,   --  local file newer than remote
      EPKG_UNKNOWN,    --  unkown keyword
      EPKG_REPOSCHEMA, --  repo DB schema incompatible version
      EPKG_ENOACCESS,  --  Insufficient privilege for action
      EPKG_INSECURE,   --  Insecure permissions on any component of $PKGDB_DIR/local.sqlite or
      --                   any of the repo database bits
      EPKG_CONFLICT,   --  A conflict between packages found
      EPKG_AGAIN,      --  Need to repeat operation
      EPKG_NOTINST,    --  Not installed
      EPKG_VITAL       --  Can not delete the package because it is vital, i.e. a kernel
     );

   type pkg_type is
     (PKG_NONE,        --  type cannot be determined
      PKG_FILE,        --  local file archive
      PKG_STREAM,      --  data read from a non-regular file (device, pipeline, unix socket etc.)
      PKG_REMOTE,      --  package available on the remote repository.
      PKG_INSTALLED,   --  locally installed package
      PKG_OLD_FILE     --  local file old archive.
     );

   type T_pkg_id        is mod 2**64;
   type T_pkg_size      is mod 2**64;
   type T_pkg_timestamp is mod 2**64;
   type T_progress_tick is mod 2**64;
   type T_EOL           is mod 2**64;

   type T_message_type is
     (PKG_MESSAGE_ALWAYS,
      PKG_MESSAGE_INSTALL,
      PKG_MESSAGE_REMOVE,
      PKG_MESSAGE_UPGRADE);

   type T_message is
      record
         contents        : Text;
         minimum_version : Text;
         maximum_version : Text;
         message_type    : T_message_type;
      end record;

   package pkg_message_crate is new CON.Vectors
     (Element_Type => T_message,
      Index_Type   => Natural);

   type T_pkg_dep is
      record
         origin       : Text;
         name         : Text;
         version      : Text;
         uid          : Text;
         locked       : Boolean;
      end record;

   package depends_crate is new CON.Vectors
     (Element_Type => T_pkg_dep,
      Index_Type   => Natural);

   type T_pkg is
      record
         direct       : Boolean;
         locked       : Boolean;
         automatic    : Boolean;
         vital        : Boolean;
         id           : T_pkg_id;
         --  scripts
         name         : Text;
         origin       : Text;
         version      : Text;
         old_version  : Text;
         maintainer   : Text;
         www          : Text;
         arch         : Text;
         abi          : Text;
         uid          : Text;
         digest       : Text;
         old_digest   : Text;
         messages     : pkg_message_crate.Vector;
         prefix       : Text;
         comment      : Text;
         desc         : Text;
         sum          : Text;
         repopath     : Text;
         reponame     : Text;
         repourl      : Text;
         reason       : Text;
         dep_formula  : Text;
         --  licenselogic
         pkgsize      : T_pkg_size;
         flatsize     : T_pkg_size;
         old_flatsize : T_pkg_size;
         timestamp    : T_pkg_timestamp;
         depends      : depends_crate.Vector;
         rdepends     : depends_crate.Vector;
         --  ...
         rootpath     : Text;
      end record;
   type T_pkg_Access is access all T_pkg;


   type T_pkg_file is
      record
         path         : Text;
         size         : T_pkg_size;
         sum          : Text;
         uname        : Text;
         gname        : Text;
         --  perm
         --  uid
         --  gid
         temppath     : Text;
         --  fflags
         --  config
         --  next, prev
      end record;

   package pkg_event_conflict_crate is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");

   --  Move to Core.Query later (?)
   package pkg_query_items_crate is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");

   type T_signature is
     (SIG_NONE,
      SIG_PUBKEY,
      SIG_FINGERPRINT);

   type T_mirror_type is
     (SRV,
      HTTP,
      NOMIRROR);

   type T_hash is
     (HASH_UNKNOWN,
      HASH_SHA256,
      HASH_BLAKE2);

   subtype T_priority is Integer;

   --  version 0 is an error, version 1 is the only existing version.
   type T_meta_version is new Natural range 0 .. 1;

   invalid_meta_version : constant T_meta_version := 0;

   type T_fingerprint is
      record
         hash_type : T_hash;
         hash      : Text;
         --  hh
      end record;

   type T_checksum_type is
      (PKG_HASH_TYPE_SHA256_BASE32,
       PKG_HASH_TYPE_SHA256_HEX,
       PKG_HASH_TYPE_BLAKE2_BASE32,
       PKG_HASH_TYPE_SHA256_RAW,
       PKG_HASH_TYPE_BLAKE2_RAW,
       PKG_HASH_TYPE_BLAKE2S_BASE32,
       PKG_HASH_TYPE_BLAKE2S_RAW,
       PKG_HASH_TYPE_UNKNOWN);

   type T_pubkey_Type is
     (rsa);

   type T_pkg_repo_meta_cert is
      record
         pubkey      : Text;
         pubkey_type : T_pubkey_Type;
         name        : Text;
      end record;

   package cert_crate is new CON.Vectors
     (Element_Type => T_pkg_repo_meta_cert,
      Index_Type   => Natural);


   --  Obsolete, but maintained for compatability
   type T_pkg_formats is (TAR, TGZ, TBZ, TXZ, TZS);

   type T_pkg_repo_flags is (REPO_FLAGS_DEFAULT, REPO_FLAGS_LIMIT_IPV4, REPO_FLAGS_LIMIT_IPV6);

   --  Use the url record from fetch bind later
   type T_http_mirror is
      record
         --  url : struct url
         urlp1 : Text;
         urlp2 : Text;
      end record;

   package http_mirror_crate is new CON.Vectors
     (Element_Type => T_http_mirror,
      Index_Type   => Natural);

   --  Move this to utils package?
   type T_dns_srvinfo is
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

   package dns_srvinfo_crate is new CON.Vectors
     (Element_Type => T_dns_srvinfo,
      Index_Type   => Natural);

   type T_pkg_repo_meta is
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
         digest_format     : T_checksum_type;
         cert              : cert_crate.Vector;
         packing_format    : T_pkg_formats := TZS;
         revision          : Integer;
         end_of_life       : T_EOL;
         version           : T_meta_version;
      end record;
   type T_pkg_repo_meta_Access is access all T_pkg_repo_meta;

   package text_crate is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");
   package sorter is new text_crate.Generic_Sorting ("<" => SU."<");

   package nvpair_crate is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Text,
      Hash            => Strings.map_hash,
      Equivalent_Keys => Strings.equivalent,
      "="             => SU."=");


   type repo_ops_variant is (binary);

   type T_pkg_repo is
      record
         name           : Text;
         url            : Text;
         pubkey         : Text;
         mirror_type    : T_mirror_type;
         signature_type : T_signature;
         fingerprints   : Text;
         trusted_fprint : T_fingerprint;
         revoked_fprint : T_fingerprint;
         meta           : T_pkg_repo_meta;
         enable         : Boolean;
         priority       : T_priority;
         flags          : T_pkg_repo_flags;
         ops_variant    : repo_ops_variant;
         env            : nvpair_crate.Map;
         sqlite_handle  : aliased sqlite_h.sqlite3_Access;
         --  ssh
         --  sshio

         --  can't use immutable records inside containers
         --  srv and http are used mutually exclusively
         srv            : dns_srvinfo_crate.Vector;
         http           : http_mirror_crate.Vector;
      end record;

   package pkg_repos_crate is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => T_pkg_repo,
      Hash            => Strings.map_hash,
      Equivalent_Keys => Strings.equivalent);

   type T_repo_priority is
      record
         reponame : Text;
         priority : T_priority;
      end record;

   --  Order to sort T_repo_priority
   function repo_priority_less_than (A, B : T_repo_priority) return Boolean;

   package pkg_repos_priority_crate is new CON.Vectors
     (Element_Type => T_repo_priority,
      Index_Type   => Natural);

   package priority_sorter is new pkg_repos_priority_crate.Generic_Sorting
     ("<" => repo_priority_less_than);

   type T_pkg_context is
      record
         eventpipe      : Unix.File_Descriptor := Unix.not_connected;
         debug_level    : ST_Debug_Level := ST_Debug_Level'First;
         developer_mode : Boolean        := False;
         pkg_rootdir    : Text;
         rootfd         : Unix.File_Descriptor := Unix.not_connected;
         cachedirfd     : Unix.File_Descriptor := Unix.not_connected;
         dbdirfd        : Unix.File_Descriptor := Unix.not_connected;
         pkg_dbdirfd    : Unix.File_Descriptor := Unix.not_connected;
      end record;

   context : T_pkg_context;

end Core.Pkg;
