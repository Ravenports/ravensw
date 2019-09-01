--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


package body Core.Checksum is

   --------------------------------------------------------------------
   --  pkg_checksum_type_from_string
   --------------------------------------------------------------------
   function pkg_checksum_type_from_string (name : String) return T_checksum_type
   is
   begin
      for ct in T_checksum_type'Range loop
         exit when ct = T_checksum_type'Last;
         if pkg_checksum_type_to_string (ct) = name then
            return ct;
         end if;
      end loop;
      return PKG_HASH_TYPE_UNKNOWN;
   end pkg_checksum_type_from_string;


   --------------------------------------------------------------------
   --  pkg_checksum_type_to_string
   --------------------------------------------------------------------
   function pkg_checksum_type_to_string (checksum_type : T_checksum_type) return String is
   begin
      case checksum_type is
         when PKG_HASH_TYPE_SHA256_BASE32  => return "sha256_base32";
         when PKG_HASH_TYPE_SHA256_HEX     => return "sha256_hex";
         when PKG_HASH_TYPE_SHA256_RAW     => return "sha256_raw";
         when PKG_HASH_TYPE_BLAKE2_BASE32  => return "blake2_base32";
         when PKG_HASH_TYPE_BLAKE2_RAW     => return "blake2_raw";
         when PKG_HASH_TYPE_BLAKE2S_BASE32 => return "blake2s_base32";
         when PKG_HASH_TYPE_BLAKE2S_RAW    => return "blake2s_raw";
         when PKG_HASH_TYPE_UNKNOWN        => return "unknown";
      end case;
   end pkg_checksum_type_to_string;

end Core.Checksum;
