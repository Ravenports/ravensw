--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Utilities is

   subtype hexrep is String (1 .. 2);

   --  unlike realpath(3), this routine does not expand symbolic links
   function pkg_absolutepath (input_path : String; fromroot : Boolean) return String;

   --  Converts an 8-bit character to a 2-digit lower-case hex string
   function char2hex (quattro : Character) return hexrep;

end Core.Utilities;
