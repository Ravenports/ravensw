--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkgtypes;

package Core.Printf is

   --  handles %n
   function format_name (pkg : Pkgtypes.A_Package) return String;

   --  handles %v
   function format_version (pkg : Pkgtypes.A_Package) return String;

   --  return the first message in the pkg's crate
   function first_message_contents (pkg : Pkgtypes.A_Package) return String;

   type string_attribute is
     (PKG_NAME,
      PKG_ORIGIN,
      PKG_VERSION,
      PKG_MAINTAINER,
      PKG_WWW,
      PKG_ARCH,
      PKG_ABI,
      PKG_UNIQUE_ID,
      PKG_DIGEST,
      PKG_PREFIX,
      PKG_COMMENT,
      PKG_DESCRIPTION,
      PKG_CHECKSUM,
      PKG_REPOPATH,
      PKG_REPONAME,
      PKG_REPOURL,
      PKG_MSG_ALWAYS,
      PKG_MSG_INSTALL,
      PKG_MSG_REMOVE,
      PKG_MSG_UPGRADE
     );

   function format_attribute
     (pkg : Pkgtypes.A_Package;
      attribute : string_attribute) return String;

   type dep_attribute is
     (DEP_ORIGIN,
      DEP_NAME,
      DEP_VERSION,
      DEP_UNIQUE_ID
     );

   function dependency_count (pkg : Pkgtypes.A_Package) return Integer;

   function format_dep_attribute
     (pkg       : Pkgtypes.A_Package;
      index     : Positive;
      attribute : dep_attribute) return String;

   function format_dep_attribute
     (pkg       : Pkgtypes.A_Package;
      name      : String;
      attribute : dep_attribute) return String;

   function option_count (pkg : Pkgtypes.A_Package) return Integer;

   type option_attribute is
     (OPT_NAME,
      OPT_VALUE);

   function format_option
     (pkg       : Pkgtypes.A_Package;
      index     : Positive;
      attribute : option_attribute) return String;

   function format_option
     (pkg       : Pkgtypes.A_Package;
      name      : String;
      attribute : option_attribute) return String;

   function category_count (pkg : Pkgtypes.A_Package) return Integer;

   function format_category (pkg : Pkgtypes.A_Package; index : Positive) return String;

   function files_count (pkg : Pkgtypes.A_Package) return Integer;

   type file_attribute is
     (FILE_PATH,
      FILE_UNAME,
      FILE_GNAME,
      FILE_CHECKSUM,
      FILE_TMPPATH,
      FILE_CONFIG_FILE
     );

   function format_file_attribute
     (pkg       : Pkgtypes.A_Package;
      index     : Positive;
      attribute : file_attribute) return String;

end Core.Printf;
