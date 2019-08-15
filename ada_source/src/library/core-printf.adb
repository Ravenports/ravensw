--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;  use Core.Strings;

package body Core.Printf is

   --------------------------------------------------------------------------------------------
   --  format_name
   --------------------------------------------------------------------------------------------
   function format_name (pkg : T_pkg) return String is
   begin
      return USS (pkg.name);
   end format_name;


   --------------------------------------------------------------------------------------------
   --  format_version
   --------------------------------------------------------------------------------------------
   function format_version (pkg : T_pkg) return String is
   begin
      return USS (pkg.version);
   end format_version;


   --------------------------------------------------------------------------------------------
   --  first_message_contents
   --------------------------------------------------------------------------------------------
   function first_message_contents (pkg : T_pkg) return String is
   begin
      if pkg.messages.Is_Empty then
         return "";
      else
         return USS (pkg.messages.First_Element.contents);
      end if;
   end first_message_contents;

end Core.Printf;
