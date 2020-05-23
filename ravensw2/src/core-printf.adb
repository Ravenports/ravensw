--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;

use Core.Strings;

package body Core.Printf is

   --------------------------------------------------------------------
   --  format_name
   --------------------------------------------------------------------
   function format_name (pkg : Pkgtypes.A_Package) return String is
   begin
      return USS (pkg.name);
   end format_name;


   --------------------------------------------------------------------
   --  format_version
   --------------------------------------------------------------------
   function format_version (pkg : Pkgtypes.A_Package) return String is
   begin
      return USS (pkg.version);
   end format_version;


   --------------------------------------------------------------------
   --  first_message_contents
   --------------------------------------------------------------------
   function first_message_contents (pkg : Pkgtypes.A_Package) return String is
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
   function format_attribute
     (pkg : Pkgtypes.A_Package;
      attribute : string_attribute) return String
   is
      message_type : Pkgtypes.A_Message_Type;
      result       : Text;

      procedure print (position : Pkgtypes.Message_Crate.Cursor);
      procedure print (position : Pkgtypes.Message_Crate.Cursor)
      is
         use type Pkgtypes.A_Message_Type;
      begin
         if Pkgtypes.Message_Crate.Element (position).message_type = message_type then
            result := Pkgtypes.Message_Crate.Element (position).contents;
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
            message_type := Pkgtypes.PKG_MESSAGE_ALWAYS;
            pkg.messages.Iterate (print'Access);
            return USS (result);
         when PKG_MSG_INSTALL =>
            message_type := Pkgtypes.PKG_MESSAGE_INSTALL;
            pkg.messages.Iterate (print'Access);
            return USS (result);
         when PKG_MSG_REMOVE  =>
            message_type := Pkgtypes.PKG_MESSAGE_REMOVE;
            pkg.messages.Iterate (print'Access);
            return USS (result);
         when PKG_MSG_UPGRADE =>
            message_type := Pkgtypes.PKG_MESSAGE_UPGRADE;
            pkg.messages.Iterate (print'Access);
            return USS (result);
      end case;
   end format_attribute;


   --------------------------------------------------------------------
   --  dependency_count
   --------------------------------------------------------------------
   function dependency_count (pkg : Pkgtypes.A_Package) return Integer is
   begin
      return Integer (pkg.depends.Length);
   end dependency_count;


   --------------------------------------------------------------------
   --  format_dep_attribute #1
   --------------------------------------------------------------------
   function format_dep_attribute
     (pkg       : Pkgtypes.A_Package;
      index     : Positive;
      attribute : dep_attribute) return String
   is
      procedure scan (position : Pkgtypes.Dependency_Crate.Cursor);

      track  : Integer := 0;
      result : Text;

      procedure scan (position : Pkgtypes.Dependency_Crate.Cursor)
      is
         item : Pkgtypes.Package_Dependency renames Pkgtypes.Dependency_Crate.Element (position);
      begin
         track := track + 1;
         if track = index then
            case attribute is
               when DEP_ORIGIN    => result := item.origin;
               when DEP_NAME      => result := item.name;
               when DEP_VERSION   => result := item.version;
               when DEP_UNIQUE_ID => result := item.uid;
            end case;
         end if;
      end scan;
   begin
      pkg.depends.Iterate (scan'Access);
      return USS (result);
   end format_dep_attribute;


   --------------------------------------------------------------------
   --  format_dep_attribute #2
   --------------------------------------------------------------------
   function format_dep_attribute
     (pkg       : Pkgtypes.A_Package;
      name      : String;
      attribute : dep_attribute) return String
   is
      result : Text;
      nametxt : Text := SUS (name);
   begin
      if pkg.depends.Contains (nametxt) then
         case attribute is
            when DEP_ORIGIN    => result := pkg.depends.Element (nametxt).origin;
            when DEP_NAME      => result := pkg.depends.Element (nametxt).name;
            when DEP_VERSION   => result := pkg.depends.Element (nametxt).version;
            when DEP_UNIQUE_ID => result := pkg.depends.Element (nametxt).uid;
         end case;
      end if;
      return USS (result);
   end format_dep_attribute;


   --------------------------------------------------------------------
   --  option_count
   --------------------------------------------------------------------
   function option_count (pkg : Pkgtypes.A_Package) return Integer is
   begin
      return Integer (pkg.options.Length);
   end option_count;


   --------------------------------------------------------------------
   --  format_option #1
   --------------------------------------------------------------------
   function format_option
     (pkg       : Pkgtypes.A_Package;
      index     : Positive;
      attribute : option_attribute) return String
   is
      procedure scan (position : Pkgtypes.Package_NVPairs.Cursor);

      track  : Integer := 0;
      result : Text;

      procedure scan (position : Pkgtypes.Package_NVPairs.Cursor) is
      begin
         track := track + 1;
         if track = index then
            case attribute is
               when OPT_VALUE => result := Pkgtypes.Package_NVPairs.Element (position);
               when OPT_NAME  => result := Pkgtypes.Package_NVPairs.Key (position);
            end case;
         end if;
      end scan;
   begin
      pkg.options.Iterate (scan'Access);
      return USS (result);
   end format_option;


   --------------------------------------------------------------------
   --  format_option #2
   --------------------------------------------------------------------
   function format_option
     (pkg       : Pkgtypes.A_Package;
      name      : String;
      attribute : option_attribute) return String
   is
      result  : Text;
      nametxt : Text := SUS (name);
   begin
      if pkg.options.Contains (nametxt) then
         case attribute is
            when OPT_VALUE => result := pkg.options.Element (nametxt);
            when OPT_NAME  => result := nametxt;
         end case;
      end if;
      return USS (result);
   end format_option;


   --------------------------------------------------------------------
   --  category_count
   --------------------------------------------------------------------
   function category_count (pkg : Pkgtypes.A_Package) return Integer is
   begin
      return Integer (pkg.categories.Length);
   end category_count;


   --------------------------------------------------------------------
   --  format_category
   --------------------------------------------------------------------
   function format_category (pkg : Pkgtypes.A_Package; index : Positive) return String
   is
      procedure scan (position : Pkgtypes.Text_Crate.Cursor);

      track  : Integer := 0;
      result : Text;

      procedure scan (position : Pkgtypes.Text_Crate.Cursor) is
      begin
         track := track + 1;
         if track = index then
            result := Pkgtypes.Text_Crate.Element (position);
         end if;
      end scan;
   begin
      pkg.categories.Iterate (scan'Access);
      return USS (result);
   end format_category;


   --------------------------------------------------------------------
   --  files_count
   --------------------------------------------------------------------
   function files_count (pkg : Pkgtypes.A_Package) return Integer is
   begin
      return Integer (pkg.files.Length);
   end files_count;


   --------------------------------------------------------------------
   --  format_file_attribute
   --------------------------------------------------------------------
   function format_file_attribute
     (pkg       : Pkgtypes.A_Package;
      index     : Positive;
      attribute : file_attribute) return String
   is
      procedure scan (position : Pkgtypes.File_Crate.Cursor);

      track  : Integer := 0;
      result : Text;

      procedure scan (position : Pkgtypes.File_Crate.Cursor)
      is
         item : Pkgtypes.Package_File renames Pkgtypes.File_Crate.Element (position);
      begin
         track := track + 1;
         if track = index then
            case attribute is
               when FILE_PATH        => result := item.path;
               when FILE_UNAME       => result := item.uname;
               when FILE_GNAME       => result := item.gname;
               when FILE_CHECKSUM    => result := item.sum;
               when FILE_TMPPATH     => result := item.tmppath;
               when FILE_CONFIG_FILE => result := item.confile;
            end case;
         end if;
      end scan;
   begin
      pkg.files.Iterate (scan'Access);
      return USS (result);
   end format_file_attribute;

end Core.Printf;
