--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Core.Strings;
with Core.Unix;

package Core.Pkgtypes is

   package CON renames Ada.Containers;

   type Package_ID         is mod 2**64;
   type Package_Size       is mod 2**64;
   type Package_Timestamp  is mod 2**64;
   type Package_Open_Flags is mod 2**3;
   type Package_Dir_Flags  is mod 2**8;

   type mode_t is new Natural range 0 .. 16#FFFF#;
   ACCESS_F_OK : constant mode_t := 0;
   ACCESS_X_OK : constant mode_t := 1;
   ACCESS_W_OK : constant mode_t := 2;
   ACCESS_R_OK : constant mode_t := 4;

   subtype gid_t is Integer;
   subtype uid_t is Integer;

   type A_Package_Type is
     (PKG_NONE,        --  type cannot be determined
      PKG_FILE,        --  local file archive
      PKG_STREAM,      --  data read from a non-regular file (device, pipeline, unix socket etc.)
      PKG_REMOTE,      --  package available on the remote repository.
      PKG_INSTALLED,   --  locally installed package
      PKG_OLD_FILE     --  local file old archive.
     );

   type License_Logic is
     (LICENSE_OR,      --  ASCII POS ('|')
      LICENSE_AND,     --  ASCII POS ('&')
      LICENSE_SINGLE   --  1
     );

   type Load_Section is
     (basic, deps, rdeps, files, scripts,
      options, dirs, categories, licenses, users,
      groups, shlibs_requires, shlibs_provided, annotations, conflicts,
      provides, requires, config_files);

   type Package_Load_Flags is array (Load_Section) of Boolean;

   type A_Message_Type is
     (PKG_MESSAGE_ALWAYS,
      PKG_MESSAGE_INSTALL,
      PKG_MESSAGE_REMOVE,
      PKG_MESSAGE_UPGRADE);

   type Package_Message is
      record
         contents        : Text;
         minimum_version : Text;
         maximum_version : Text;
         message_type    : A_Message_Type;
      end record;

   package Message_Crate is new CON.Vectors
     (Element_Type => Package_Message,
      Index_Type   => Natural);

   type Package_Dependency is
      record
         origin       : Text;
         name         : Text;
         version      : Text;
         uid          : Text;
         locked       : Boolean;
      end record;

   package Dependency_Crate is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Package_Dependency,
      Hash            => Strings.map_hash,
      Equivalent_Keys => Strings.equivalent);

   package Reverse_Dependency_Crate is new CON.Vectors
     (Element_Type => Package_Dependency,
      Index_Type   => Natural);

   type Package_Conflict_Type is
     (PKG_CONFLICT_ALL,
      PKG_CONFLICT_REMOTE_LOCAL,
      PKG_CONFLICT_REMOTE_REMOTE,
      PKG_CONFLICT_LOCAL_LOCAL);

   type Package_Conflict is
      record
         uid     : Text;
         digest  : Text;
         contype : Package_Conflict_Type;
      end record;

   package Conflicts_Crate is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Package_Conflict,
      Hash            => Strings.map_hash,
      Equivalent_Keys => Strings.equivalent);

   type A_Timespec is
      record
         tv_sec  : Package_Timestamp;
         tv_nsec : Package_Timestamp;
      end record;

   type Package_Directory is
      record
         path    : Text;
         uname   : Text;
         gname   : Text;
         perm    : mode_t;
         gid     : gid_t;
         uid     : uid_t;
         fflags  : Package_Dir_Flags;
         noattrs : Boolean;
         stamp   : A_Timespec;
      end record;

   package Directory_Crate is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Package_Directory,
      Hash            => Strings.map_hash,
      Equivalent_Keys => Strings.equivalent);

   type Package_File is
      record
         path    : Text;
         uname   : Text;
         gname   : Text;
         perm    : mode_t;
         gid     : gid_t;
         uid     : uid_t;
         fflags  : Package_Dir_Flags;
         stamp   : A_Timespec;

         size    : Package_Size;
         sum     : Text;
         tmppath : Text;
         confile : Text;  --  path to configure file, index to map
      end record;

   package File_Crate is new CON.Vectors
     (Element_Type => Package_File,
      Index_Type   => Natural);

   type Merge_Status is
     (MERGE_NOTNEEDED,
      MERGE_FAILED,
      MERGE_SUCCESS);

   type Package_Config_File is
      record
         path       : Text;
         content    : Text;
         newcontent : Text;
         status     : Merge_Status;
      end record;

   package Config_File_Crate is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Package_Config_File,
      Hash            => Strings.map_hash,
      Equivalent_Keys => Strings.equivalent);

   type Package_Script is
     (PKG_SCRIPT_PRE_INSTALL,
      PKG_SCRIPT_POST_INSTALL,
      PKG_SCRIPT_PRE_DEINSTALL,
      PKG_SCRIPT_POST_DEINSTALL,
      PKG_SCRIPT_PRE_UPGRADE,
      PKG_SCRIPT_POST_UPGRADE,
      PKG_SCRIPT_INSTALL,
      PKG_SCRIPT_DEINSTALL,
      PKG_SCRIPT_UPGRADE);

   type Package_Script_Set is array (Package_Script) of Text;

   package Text_Crate is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");
   package sorter is new Text_Crate.Generic_Sorting ("<" => SU."<");

   package Package_NVPairs is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Text,
      Hash            => Strings.map_hash,
      Equivalent_Keys => Strings.equivalent,
      "="             => SU."=");

   type A_Package is
      record
         direct       : Boolean;
         locked       : Boolean;
         automatic    : Boolean;
         vital        : Boolean;
         id           : Package_ID;
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
         messages     : Message_Crate.Vector;
         prefix       : Text;
         comment      : Text;
         desc         : Text;
         sum          : Text;
         repopath     : Text;
         reponame     : Text;
         repourl      : Text;
         reason       : Text;
         licenselogic : License_Logic;
         pkgsize      : Package_Size;
         flatsize     : Package_Size;
         old_flatsize : Package_Size;
         timestamp    : Package_Timestamp;
         sections     : Package_Load_Flags;
         depends      : Dependency_Crate.Map;
         rdepends     : Reverse_Dependency_Crate.Vector;
         categories   : Text_Crate.Vector;
         licenses     : Text_Crate.Vector;
         users        : Text_Crate.Vector;
         groups       : Text_Crate.Vector;
         shlibs_reqd  : Text_Crate.Vector;
         shlibs_prov  : Text_Crate.Vector;
         provides     : Text_Crate.Vector;
         requires     : Text_Crate.Vector;
         conflicts    : Conflicts_Crate.Map;
         options      : Package_NVPairs.Map;
         annotations  : Package_NVPairs.Map;
         scripts      : Package_Script_Set;
         dirs         : Directory_Crate.Map;
         files        : File_Crate.Vector;
         config_files : Config_File_Crate.Map;
         --  ...
         rootpath     : Text;
         rootfd       : Unix.File_Descriptor := Unix.not_connected;
         package_type : A_Package_Type       := PKG_FILE;
      end record;
   type A_Package_Access is access all A_Package;

end Core.Pkgtypes;
