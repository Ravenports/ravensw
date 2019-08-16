--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers.Vectors;
with Core.Unix;

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
      EPKG_VITAL       -- Can not delete the package because it is vital, i.e. a kernel
     );

   type T_pkg_id        is mod 2**64;
   type T_pkg_size      is mod 2**64;
   type T_pkg_timestamp is mod 2**64;
   type T_progress_tick is mod 2**64;

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

   type T_pkg_context is
      record
         eventpipe      : Unix.File_Descriptor := Unix.not_connected;
         debug_level    : ST_Debug_Level := ST_Debug_Level'First;
         developer_mode : Boolean        := False;
         pkg_rootdir    : Text;
         rootfd         : Unix.File_Descriptor;
         cachedirfd     : Unix.File_Descriptor;
         dbdirfd        : Unix.File_Descriptor;
         pkg_dbdirfd    : Unix.File_Descriptor;
      end record;

   context : T_pkg_context;

end Core.Pkg;
