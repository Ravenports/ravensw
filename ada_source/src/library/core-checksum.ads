--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;     use Core.Pkg;
with Core.Strings; use Core.Strings;

package Core.Checksum is

   function pkg_checksum_type_from_string (name : String) return T_checksum_type;

   function pkg_checksum_type_to_string (checksum_type : T_checksum_type) return String;

   function pkg_checksum_is_valid (cksum : Text) return Boolean;

private

   PKG_CKSUM_SEPARATOR      : constant String (1 .. 1) := "$";
   PKG_CHECKSUM_CUR_VERSION : constant Integer := 2;

end Core.Checksum;
