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

end Core.Printf;
