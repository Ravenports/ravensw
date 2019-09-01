--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;  use Core.Pkg;

package Core.Checksum is

   function pkg_checksum_type_from_string (name : String) return T_checksum_type;

   function pkg_checksum_type_to_string (checksum_type : T_checksum_type) return String;


end Core.Checksum;
