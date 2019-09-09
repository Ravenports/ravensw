--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;
with Core.Pkg;  use Core.Pkg;

package Core.Atf is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   -------------------------
   --  CHECKSUM ROUTINES  --
   -------------------------

   function pkg_checksum_file (path : ICS.chars_ptr; checksum_type : T_checksum_type)
                               return ICS.chars_ptr;
   pragma Export (C, pkg_checksum_file);

   function pkg_checksum_symlink (path : ICS.chars_ptr; checksum_type : T_checksum_type)
                                  return ICS.chars_ptr;
   pragma Export (C, pkg_checksum_symlink);

   function pkg_checksum_validate_file (path : ICS.chars_ptr; sum : ICS.chars_ptr) return IC.int;
   pragma Export (C, pkg_checksum_validate_file);

   function pkg_checksum_generate_file (path : ICS.chars_ptr; checksum_type : T_checksum_type)
                                        return ICS.chars_ptr;
   pragma Export (C, pkg_checksum_generate_file);

end Core.Atf;
