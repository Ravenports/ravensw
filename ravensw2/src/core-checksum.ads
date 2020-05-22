--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers.Vectors;
with Interfaces.C.Extensions;

with Core.Strings; use Core.Strings;
with Core.Unix;

package Core.Checksum is

   --  For ATF test suite (see core-atf.ads)
   --  ----------------------------
   --  checksum_symlink
   --  checksum_validate_file
   --  checksum_generate_file
   --  checksum_file

   package CON renames Ada.Containers;

   type A_Checksum_Type is
      (HASH_TYPE_SHA256_BASE32,
       HASH_TYPE_SHA256_HEX,
       HASH_TYPE_BLAKE2_BASE32,
       HASH_TYPE_SHA256_RAW,
       HASH_TYPE_BLAKE2_RAW,
       HASH_TYPE_BLAKE2S_BASE32,
       HASH_TYPE_BLAKE2S_RAW,
       HASH_TYPE_UNKNOWN);
   pragma Convention (C, A_Checksum_Type);

   function checksum_type_from_string (name : String) return A_Checksum_Type;

   function checksum_type_to_string (checksum_type : A_Checksum_Type) return String;

   function checksum_is_valid (cksum : Text) return Boolean;

   function checksum_file (path : String; checksum_type : A_Checksum_Type) return String;

   function checksum_fd
     (fd : Unix.File_Descriptor;
      checksum_type : A_Checksum_Type)
      return String;

   function checksum_fileat
     (rootfd        : Unix.File_Descriptor;
      path          : String;
      checksum_type : A_Checksum_Type) return String;

   function checksum_symlink (path : String; checksum_type : A_Checksum_Type) return String;

   function checksum_symlinkat
     (fd            : Unix.File_Descriptor;
      relative_path : String;
      checksum_type : A_Checksum_Type) return String;

   function checksum_validate_file (path : String; sum : String) return Boolean;

   function checksum_generate_file (path : String; checksum_type : A_Checksum_Type)
                                        return String;

   function checksum_data (instr : String;  checksum_type : A_Checksum_Type) return String;

private

   CHECKSUM_SEPARATOR   : constant String (1 .. 1) := "$";
   CHECKSUM_CUR_VERSION : constant Integer := 2;

   type checksum_entry is
      record
         field : Text;
         value : Text;
      end record;

   package checksum_entry_crate is new CON.Vectors
     (Element_Type => checksum_entry,
      Index_Type   => Natural);

   function checksum_hash_sha256  (entries : checksum_entry_crate.Vector) return String;
   function checksum_hash_blake2b (entries : checksum_entry_crate.Vector) return String;
   function checksum_hash_blake2s (entries : checksum_entry_crate.Vector) return String;

   function checksum_hash_file         (fd : Unix.File_Descriptor;
                                            checksum_type : A_Checksum_Type) return String;
   function checksum_hash_sha256_file  (fd : Unix.File_Descriptor) return String;
   function checksum_hash_blake2b_file  (fd : Unix.File_Descriptor) return String;
   function checksum_hash_blake2s_file (fd : Unix.File_Descriptor) return String;

   function checksum_encode        (plain : String;
                                        checksum_type : A_Checksum_Type) return String;
   function checksum_encode_base32 (plain : String) return String;
   function checksum_encode_hex    (plain : String) return String;

   function checksum_hash_bulk         (plain : String;
                                            checksum_type : A_Checksum_Type) return String;
   function checksum_hash_sha256_bulk  (plain : String) return String;
   function checksum_hash_blake2b_bulk (plain : String) return String;
   function checksum_hash_blake2s_bulk (plain : String) return String;

   function checksum_symlink_readlink
     (link_path     : String;
      checksum_type : A_Checksum_Type) return String;

   function checksum_get_type_helper (frag : String) return A_Checksum_Type;
   function checksum_get_type        (cksum : String) return A_Checksum_Type;
   function checksum_file_get_type   (cksum : String) return A_Checksum_Type;

end Core.Checksum;
