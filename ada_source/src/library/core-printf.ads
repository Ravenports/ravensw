--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;  use Core.Pkg;

package Core.Printf is

   --  handles %n
   function format_name (pkg : T_pkg) return String;

   --  handles %v
   function format_version (pkg : T_pkg) return String;

   --  return the first message in the pkg's crate
   function first_message_contents (pkg : T_pkg) return String;

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

   function format_attribute (pkg : T_pkg; attribute : string_attribute) return String;

   type dep_attribute is
     (DEP_ORIGIN,
      DEP_NAME,
      DEP_VERSION,
      DEP_UNIQUE_ID
     );

   function dependency_count (pkg : T_pkg) return Integer;

   function format_dep_attribute
     (pkg       : T_pkg;
      index     : Natural;
      attribute : dep_attribute) return String;

end Core.Printf;
