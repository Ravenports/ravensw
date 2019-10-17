--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;  use Core.Strings;

package body Core.Printf is

   --------------------------------------------------------------------
   --  format_name
   --------------------------------------------------------------------
   function format_name (pkg : T_pkg) return String is
   begin
      return USS (pkg.name);
   end format_name;


   --------------------------------------------------------------------
   --  format_version
   --------------------------------------------------------------------
   function format_version (pkg : T_pkg) return String is
   begin
      return USS (pkg.version);
   end format_version;


   --------------------------------------------------------------------
   --  first_message_contents
   --------------------------------------------------------------------
   function first_message_contents (pkg : T_pkg) return String is
   begin
      if pkg.messages.Is_Empty then
         return "";
      else
         return USS (pkg.messages.First_Element.contents);
      end if;
   end first_message_contents;


   --------------------------------------------------------------------
   --  format_attribute
   --------------------------------------------------------------------
   function format_attribute (pkg : T_pkg; attribute : string_attribute) return String
   is
      message_type : T_message_type;
      result       : Text;

      procedure print (position : pkg_message_crate.Cursor);
      procedure print (position : pkg_message_crate.Cursor) is
      begin
         if pkg_message_crate.Element (position).message_type = message_type then
            result := pkg_message_crate.Element (position).contents;
         end if;
      end print;
   begin
      case attribute is
         when PKG_NAME        => return USS (pkg.name);
         when PKG_ORIGIN      => return USS (pkg.origin);
         when PKG_VERSION     => return USS (pkg.version);
         when PKG_MAINTAINER  => return USS (pkg.maintainer);
         when PKG_WWW         => return USS (pkg.www);
         when PKG_ARCH        => return USS (pkg.arch);
         when PKG_ABI         => return USS (pkg.abi);
         when PKG_UNIQUE_ID   => return USS (pkg.uid);
         when PKG_DIGEST      => return USS (pkg.digest);
         when PKG_PREFIX      => return USS (pkg.prefix);
         when PKG_COMMENT     => return USS (pkg.comment);
         when PKG_DESCRIPTION => return USS (pkg.desc);
         when PKG_CHECKSUM    => return USS (pkg.sum);
         when PKG_REPOPATH    => return USS (pkg.repopath);
         when PKG_REPONAME    => return USS (pkg.reponame);
         when PKG_REPOURL     => return USS (pkg.repourl);
         when PKG_MSG_ALWAYS  =>
            message_type := PKG_MESSAGE_ALWAYS;
            pkg.messages.Iterate (print'Access);
            return USS (result);
         when PKG_MSG_INSTALL =>
            message_type := PKG_MESSAGE_INSTALL;
            pkg.messages.Iterate (print'Access);
            return USS (result);
         when PKG_MSG_REMOVE  =>
            message_type := PKG_MESSAGE_REMOVE;
            pkg.messages.Iterate (print'Access);
            return USS (result);
         when PKG_MSG_UPGRADE =>
            message_type := PKG_MESSAGE_UPGRADE;
            pkg.messages.Iterate (print'Access);
            return USS (result);
      end case;
   end format_attribute;

end Core.Printf;
