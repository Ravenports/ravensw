--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

--  with Ada.Streams.Stream_IO;
with Interfaces;
with ssl;

with Core.Event;
with Core.Utilities;

package body Core.Checksum is

   package ITF renames Interfaces;
   --  package SIO renames Ada.Streams.Stream_IO;

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


   --------------------------------------------------------------------
   --  pkg_checksum_is_valid
   --------------------------------------------------------------------
   function pkg_checksum_is_valid (cksum : Text) return Boolean
   is
      sum : String := USS (cksum);
      testversion : Integer;
      testenum    : Integer;
      --  format: {PKG_CHECKSUM_CUR_VERSION}${type}$
   begin
      if sum'Length < 4 then
         return False;
      end if;

      if count_char (sum, PKG_CKSUM_SEPARATOR (1)) /= 2 then
         return False;
      end if;

      testversion := Integer'Value (specific_field (sum, 1, PKG_CKSUM_SEPARATOR));
      if testversion /= PKG_CHECKSUM_CUR_VERSION then
         return False;
      end if;

      testenum := Integer'Value (specific_field (sum, 2, PKG_CKSUM_SEPARATOR));
      if testenum < 0 or else testenum > T_checksum_type'Pos (T_checksum_type'Last) then
         return False;
      end if;

      return True;

   exception
      when Constraint_Error =>
         return False;
   end pkg_checksum_is_valid;


   --------------------------------------------------------------------
   --  pkg_checksum_file
   --------------------------------------------------------------------
   function pkg_checksum_file (path : String; checksum_type : T_checksum_type) return String
   is
      fd : Unix.File_Descriptor;
      R  : Unix.T_Open_Flags := (RDONLY => False, others => True);
   begin
      fd := Unix.open_file (path, R);
      if Unix.file_connected (fd) then
         declare
            result : constant String := pkg_checksum_fd (fd, checksum_type);
         begin
            if Unix.close_file (fd) then
               null;
            end if;
            return result;
         end;
      else
         Event.pkg_emit_errno (function_name => SUS ("pkg_checksum_file"),
                               arguments     => SUS (path & ", readonly"),
                               error_number  => Unix.errno);
         return "";
      end if;

   end pkg_checksum_file;


   --------------------------------------------------------------------
   --  pkg_checksum_fd
   --------------------------------------------------------------------
   function pkg_checksum_fd
     (fd : Unix.File_Descriptor;
      checksum_type : T_checksum_type)
      return String
   is
   begin
      if checksum_type = PKG_HASH_TYPE_UNKNOWN or else
        not Unix.file_connected (fd)
      then
         return "";
      end if;

      declare
         cksum : constant String := pkg_checksum_hash_file (fd, checksum_type);
      begin
         return pkg_checksum_encode (cksum, checksum_type);
      end;
   end pkg_checksum_fd;


   --------------------------------------------------------------------
   --  pkg_checksum_hash_sha256
   --------------------------------------------------------------------
   function pkg_checksum_hash_sha256 (entries : checksum_entry_crate.Vector) return String
   is
      procedure add (position : checksum_entry_crate.Cursor);

      sign_ctx : aliased ssl.SHA256_CTX;

      procedure add (position : checksum_entry_crate.Cursor)
      is
         item : pkg_checksum_entry renames checksum_entry_crate.Element (position);
      begin
         ssl.sha256_update (sign_ctx'Unchecked_Access, USS (item.field));
         ssl.sha256_update (sign_ctx'Unchecked_Access, USS (item.value));
      end add;
   begin
      ssl.sha256_init (sign_ctx'Unchecked_Access);
      entries.Iterate (add'Access);
      return ssl.sha256_final (sign_ctx'Unchecked_Access);
   end pkg_checksum_hash_sha256;


   --------------------------------------------------------------------
   --  pkg_checksum_hash_sha256_bulk
   --------------------------------------------------------------------
   function pkg_checksum_hash_sha256_bulk (plain : String) return String
   is
      sign_ctx : aliased ssl.SHA256_CTX;
   begin
      ssl.sha256_init (sign_ctx'Unchecked_Access);
      ssl.sha256_update (sign_ctx'Unchecked_Access, plain);
      return ssl.sha256_final (sign_ctx'Unchecked_Access);
   end pkg_checksum_hash_sha256_bulk;


   --------------------------------------------------------------------
   --  pkg_checksum_hash_sha256_file
   --------------------------------------------------------------------
   function pkg_checksum_hash_sha256_file  (fd : Unix.File_Descriptor) return String
   is
      sign_ctx   : aliased ssl.SHA256_CTX;
      chunk_size : constant Natural := 16 * 1024;
   begin
      ssl.sha256_init (sign_ctx'Unchecked_Access);
      loop
         declare
            chunk : constant String := Unix.read_fd (fd, chunk_size);
         begin
            exit when chunk'Length = 0;
            ssl.sha256_update (sign_ctx'Unchecked_Access, chunk);
         end;
      end loop;
      return ssl.sha256_final (sign_ctx'Unchecked_Access);
   end pkg_checksum_hash_sha256_file;

   --------------------------------------------------------------------
   --  pkg_checksum_hash_blake2
   --------------------------------------------------------------------
   function pkg_checksum_hash_blake2 (entries : checksum_entry_crate.Vector) return String
        is
   begin
      --  TODO:
      return "";
   end pkg_checksum_hash_blake2;


   --------------------------------------------------------------------
   --  pkg_checksum_hash_blake2s
   --------------------------------------------------------------------
   function pkg_checksum_hash_blake2s (entries : checksum_entry_crate.Vector) return String
   is
   begin
      --  TODO:
      return "";
   end pkg_checksum_hash_blake2s;


   --------------------------------------------------------------------
   --  pkg_checksum_hash_blake2_file
   --------------------------------------------------------------------
   function pkg_checksum_hash_blake2_file  (fd : Unix.File_Descriptor) return String is
   begin
      --  TODO:
      return "";
   end pkg_checksum_hash_blake2_file;


   --------------------------------------------------------------------
   --  pkg_checksum_hash_blake2s_file
   --------------------------------------------------------------------
   function pkg_checksum_hash_blake2s_file (fd : Unix.File_Descriptor) return String is
   begin
      --  TODO:
      return "";
   end pkg_checksum_hash_blake2s_file;


   --------------------------------------------------------------------
   --  pkg_checksum_hash_blake2s_file
   --------------------------------------------------------------------
   function pkg_checksum_hash_file (fd : Unix.File_Descriptor;
                                    checksum_type : T_checksum_type) return String is
   begin
      case checksum_type is
         when PKG_HASH_TYPE_SHA256_BASE32  |
              PKG_HASH_TYPE_SHA256_RAW     |
              PKG_HASH_TYPE_SHA256_HEX     => return pkg_checksum_hash_sha256_file (fd);
         when PKG_HASH_TYPE_BLAKE2_BASE32  |
              PKG_HASH_TYPE_BLAKE2_RAW     => return pkg_checksum_hash_blake2_file (fd);
         when PKG_HASH_TYPE_BLAKE2S_BASE32 |
              PKG_HASH_TYPE_BLAKE2S_RAW    => return pkg_checksum_hash_blake2s_file (fd);
         when PKG_HASH_TYPE_UNKNOWN        => return "";
      end case;
   end pkg_checksum_hash_file;


   --------------------------------------------------------------------
   --  pkg_checksum_encode
      --------------------------------------------------------------------
   function pkg_checksum_encode (plain : String; checksum_type : T_checksum_type) return String is
   begin
      case checksum_type is
         when PKG_HASH_TYPE_SHA256_BASE32  |
              PKG_HASH_TYPE_BLAKE2_BASE32  |
              PKG_HASH_TYPE_BLAKE2S_BASE32 => return pkg_checksum_encode_base32 (plain);
         when PKG_HASH_TYPE_SHA256_HEX     => return pkg_checksum_encode_hex (plain);
         when PKG_HASH_TYPE_SHA256_RAW     |
              PKG_HASH_TYPE_BLAKE2_RAW     |
              PKG_HASH_TYPE_BLAKE2S_RAW    => return plain;
         when PKG_HASH_TYPE_UNKNOWN        => return "";
      end case;
   end pkg_checksum_encode;


   --------------------------------------------------------------------
   --  pkg_checksum_encode_base32
   --------------------------------------------------------------------
   function pkg_checksum_encode_base32 (plain : String) return String
   is
      type fullbyte is mod 2 ** 8;
      subtype bits5 is Natural range 0 .. 4;

      --  We use here z-base32 encoding described here:
      --  http://philzimmermann.com/docs/human-oriented-base-32-encoding.txt
      b32 : constant String := "ybndrfg8ejkmcpqxot1uwisza345h769";

      scenario  : bits5;
      mylast    : Natural := (plain'Length * 8) / 5;
      result    : String (1 .. mylast);
      index     : Natural := result'First;
      rawbytes  : array (0 .. plain'Length - 1) of fullbyte;
      x         : fullbyte;
      x2        : fullbyte;
      remain    : fullbyte;
      remaining : Boolean;
   begin
      declare
         b : Natural := rawbytes'First;
      begin
         for k in plain'Range loop
            rawbytes (b) := fullbyte (Character'Pos (plain (k)));
         end loop;
      end;

      for k in rawbytes'Range loop
         scenario := k mod 5;
         case scenario is
            when 0 =>
               --  8 bits of input and 3 to remain
               x := rawbytes (k);
               remain := fullbyte (ITF.Shift_Right (ITF.Unsigned_8 (rawbytes (k)), 5));
               result (index) := b32 (b32'First + Integer (x and 16#1F#));
               index := index + 1;
               remaining := True;
            when 1 =>
               --  11 bits of input, 1 to remain
               x  := (remain or fullbyte (ITF.Shift_Left (ITF.Unsigned_8 (rawbytes (k)), 3)));
               x2 := fullbyte (ITF.Shift_Right (ITF.Unsigned_8 (x), 5));
               result (index) := b32 (b32'First + Integer (x and 16#1F#));
               index := index + 1;
               result (index) := b32 (b32'First + Integer (x2 and 16#1F#));
               index := index + 1;
               remain := fullbyte (ITF.Shift_Right (ITF.Unsigned_8 (x), 10));
            when 2 =>
               --  9 bits of input, 4 to remain
               x  := (remain or fullbyte (ITF.Shift_Left (ITF.Unsigned_8 (rawbytes (k)), 1)));
               result (index) := b32 (b32'First + Integer (x and 16#1F#));
               index := index + 1;
               remain := fullbyte (ITF.Shift_Right (ITF.Unsigned_8 (x), 5));
            when 3 =>
               --  12 bits of input, 2 to remain
               x  := (remain or fullbyte (ITF.Shift_Left (ITF.Unsigned_8 (rawbytes (k)), 4)));
               x2 := fullbyte (ITF.Shift_Right (ITF.Unsigned_8 (x), 5));
               result (index) := b32 (b32'First + Integer (x and 16#1F#));
               index := index + 1;
               result (index) := b32 (b32'First + Integer (x2 and 16#1F#));
               index := index + 1;
               remain := (fullbyte (ITF.Shift_Right (ITF.Unsigned_8 (x), 10)) and 16#3#);
            when 4 =>
               --  10 bits of output, nothing to remain
               x  := (remain or fullbyte (ITF.Shift_Left (ITF.Unsigned_8 (rawbytes (k)), 2)));
               x2 := fullbyte (ITF.Shift_Right (ITF.Unsigned_8 (x), 5));
               result (index) := b32 (b32'First + Integer (x and 16#1F#));
               index := index + 1;
               result (index) := b32 (b32'First + Integer (x2 and 16#1F#));
               index := index + 1;
               remaining := False;
         end case;
      end loop;

      if remaining then
         result (index) := b32 (b32'First + Integer (remain));
      end if;

      return result;
   end pkg_checksum_encode_base32;


   --------------------------------------------------------------------
   --  pkg_checksum_encode_hex
   --------------------------------------------------------------------
   function pkg_checksum_encode_hex (plain : String) return String
   is
      result : String (1 .. plain'Length * 2);
      index  : Natural := 1;
   begin
      for x in plain'Range loop
         result (index .. index + 1) := Utilities.char2hex (plain (x));
         index := index + 2;
      end loop;

      return result;
   end pkg_checksum_encode_hex;


end Core.Checksum;
