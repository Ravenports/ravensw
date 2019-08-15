--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;

package Core.Unix is

   package IC renames Interfaces.C;

   --  strerror from libc
   function strerror (errno : Integer) return String;

private

   function C_Strerror (Errnum : IC.int) return IC.Strings.chars_ptr;
   pragma Import (C, C_Strerror, "strerror");

end Core.Unix;
