--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Utilities is

   --  unlike realpath(3), this routine does not expand symbolic links
   function pkg_absolutepath (input_path : String; fromroot: Boolean) return String;

end Core.Utilities;
