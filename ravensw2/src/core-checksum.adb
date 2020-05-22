--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces;
with SSL;
with blake2;

with Core.Event;
with Core.Utilities;

package body Core.Checksum is

   package ITF renames Interfaces;

   --------------------------------------------------------------------
   --  checksum_type_from_string
   --------------------------------------------------------------------
   function checksum_type_from_string (name : String) return A_Checksum_Type
   is
   begin
      for ct in A_Checksum_Type'Range loop
         exit when ct = A_Checksum_Type'Last;
         if checksum_type_to_string (ct) = name then
            return ct;
         end if;
      end loop;
      return HASH_TYPE_UNKNOWN;
   end checksum_type_from_string;


   --------------------------------------------------------------------
   --  checksum_type_to_string
   --------------------------------------------------------------------
   function checksum_type_to_string (checksum_type : A_Checksum_Type) return String is
   begin
      case checksum_type is
         when HASH_TYPE_SHA256_BASE32  => return "sha256_base32";
         when HASH_TYPE_SHA256_HEX     => return "sha256_hex";
         when HASH_TYPE_SHA256_RAW     => return "sha256_raw";
         when HASH_TYPE_BLAKE2_BASE32  => return "blake2_base32";
         when HASH_TYPE_BLAKE2_RAW     => return "blake2_raw";
         when HASH_TYPE_BLAKE2S_BASE32 => return "blake2s_base32";
         when HASH_TYPE_BLAKE2S_RAW    => return "blake2s_raw";
         when HASH_TYPE_UNKNOWN        => return "unknown";
      end case;
   end checksum_type_to_string;


   --------------------------------------------------------------------
   --  checksum_is_valid
   --------------------------------------------------------------------
   function checksum_is_valid (cksum : Text) return Boolean
   is
      sum : String := USS (cksum);
      testversion : Integer;
      testenum    : Integer;
      --  format: {CHECKSUM_CUR_VERSION}${type}$
   begin
      if sum'Length < 4 then
         return False;
      end if;

      if count_char (sum, CHECKSUM_SEPARATOR (1)) /= 2 then
         return False;
      end if;

      testversion := Integer'Value (specific_field (sum, 1, CHECKSUM_SEPARATOR));
      if testversion /= CHECKSUM_CUR_VERSION then
         return False;
      end if;

      testenum := Integer'Value (specific_field (sum, 2, CHECKSUM_SEPARATOR));
      if testenum < 0 or else testenum > A_Checksum_Type'Pos (A_Checksum_Type'Last) then
         return False;
      end if;

      return True;

   exception
      when Constraint_Error =>
         return False;
   end checksum_is_valid;


   --------------------------------------------------------------------
   --  checksum_file
   --------------------------------------------------------------------
   function checksum_file (path : String; checksum_type : A_Checksum_Type) return String
   is
      fd : Unix.File_Descriptor;
      R  : Unix.T_Open_Flags := (RDONLY => True, others => False);
   begin
      fd := Unix.open_file (path, R);
      if Unix.file_connected (fd) then
         declare
            result : constant String := checksum_fd (fd, checksum_type);
         begin
            if Unix.close_file (fd) then
               null;
            end if;
            return result;
         end;
      else
         Event.emit_errno ("checksum_file", path & ", readonly", Unix.errno);
         return "";
      end if;

   end checksum_file;


   --------------------------------------------------------------------
   --  checksum_fd
   --------------------------------------------------------------------
   function checksum_fd
     (fd : Unix.File_Descriptor;
      checksum_type : A_Checksum_Type)
      return String
   is
   begin
      if checksum_type = HASH_TYPE_UNKNOWN or else
        not Unix.file_connected (fd)
      then
         return "";
      end if;

      declare
         cksum : constant String := checksum_hash_file (fd, checksum_type);
      begin
         return checksum_encode (cksum, checksum_type);
      end;
   end checksum_fd;


   --------------------------------------------------------------------
   --  checksum_hash_sha256
   --------------------------------------------------------------------
   function checksum_hash_sha256 (entries : checksum_entry_crate.Vector) return String
   is
      procedure add (position : checksum_entry_crate.Cursor);

      sign_ctx : aliased SSL.SHA256_CTX;

      procedure add (position : checksum_entry_crate.Cursor)
      is
         item : checksum_entry renames checksum_entry_crate.Element (position);
      begin
         SSL.sha256_update (sign_ctx'Unchecked_Access, USS (item.field));
         SSL.sha256_update (sign_ctx'Unchecked_Access, USS (item.value));
      end add;
   begin
      SSL.sha256_init (sign_ctx'Unchecked_Access);
      entries.Iterate (add'Access);
      return SSL.sha256_final (sign_ctx'Unchecked_Access);
   end checksum_hash_sha256;


   --------------------------------------------------------------------
   --  checksum_hash_sha256_bulk
   --------------------------------------------------------------------
   function checksum_hash_sha256_bulk (plain : String) return String
   is
      sign_ctx : aliased SSL.SHA256_CTX;
   begin
      SSL.sha256_init (sign_ctx'Unchecked_Access);
      SSL.sha256_update (sign_ctx'Unchecked_Access, plain);
      return SSL.sha256_final (sign_ctx'Unchecked_Access);
   end checksum_hash_sha256_bulk;


   --------------------------------------------------------------------
   --  checksum_hash_sha256_file
   --------------------------------------------------------------------
   function checksum_hash_sha256_file  (fd : Unix.File_Descriptor) return String
   is
      sign_ctx   : aliased SSL.SHA256_CTX;
      chunk_size : constant Natural := 16 * 1024;
   begin
      SSL.sha256_init (sign_ctx'Unchecked_Access);
      loop
         declare
            chunk : constant String := Unix.read_fd (fd, chunk_size);
         begin
            exit when chunk'Length = 0;
            SSL.sha256_update (sign_ctx'Unchecked_Access, chunk);
         end;
      end loop;
      return SSL.sha256_final (sign_ctx'Unchecked_Access);
   end checksum_hash_sha256_file;


   --------------------------------------------------------------------
   --  checksum_hash_blake2b
   --------------------------------------------------------------------
   function checksum_hash_blake2b (entries : checksum_entry_crate.Vector) return String
   is
      procedure add (position : checksum_entry_crate.Cursor);

      sign_ctx : aliased blake2.blake2b_state;

      procedure add (position : checksum_entry_crate.Cursor)
      is
         item : checksum_entry renames checksum_entry_crate.Element (position);
      begin
         blake2.blake2b_update (sign_ctx'Unchecked_Access, USS (item.field));
         blake2.blake2b_update (sign_ctx'Unchecked_Access, USS (item.value));
      end add;
   begin
      blake2.blake2b_init (sign_ctx'Unchecked_Access);
      entries.Iterate (add'Access);
      return blake2.blake2b_final (sign_ctx'Unchecked_Access);
   end checksum_hash_blake2b;



   --------------------------------------------------------------------
   --  checksum_hash_blake2b_file
   --------------------------------------------------------------------
   function checksum_hash_blake2b_file  (fd : Unix.File_Descriptor) return String
   is
      sign_ctx   : aliased blake2.blake2b_state;
      chunk_size : constant Natural := 16 * 1024;
   begin
      blake2.blake2b_init (sign_ctx'Unchecked_Access);
      loop
         declare
            chunk : constant String := Unix.read_fd (fd, chunk_size);
         begin
            exit when chunk'Length = 0;
            blake2.blake2b_update (sign_ctx'Unchecked_Access, chunk);
         end;
      end loop;
      return blake2.blake2b_final (sign_ctx'Unchecked_Access);
   end checksum_hash_blake2b_file;


   --------------------------------------------------------------------
   --  checksum_hash_blake2s
   --------------------------------------------------------------------
   function checksum_hash_blake2s (entries : checksum_entry_crate.Vector) return String
   is
      procedure add (position : checksum_entry_crate.Cursor);

      sign_ctx : aliased blake2.blake2s_state;

      procedure add (position : checksum_entry_crate.Cursor)
      is
         item : checksum_entry renames checksum_entry_crate.Element (position);
      begin
         blake2.blake2s_update (sign_ctx'Unchecked_Access, USS (item.field));
         blake2.blake2s_update (sign_ctx'Unchecked_Access, USS (item.value));
      end add;
   begin
      blake2.blake2s_init (sign_ctx'Unchecked_Access);
      entries.Iterate (add'Access);
      return blake2.blake2s_final (sign_ctx'Unchecked_Access);
   end checksum_hash_blake2s;


   --------------------------------------------------------------------
   --  checksum_hash_blake2s_file
   --------------------------------------------------------------------
   function checksum_hash_blake2s_file (fd : Unix.File_Descriptor) return String
   is
      sign_ctx   : aliased blake2.blake2s_state;
      chunk_size : constant Natural := 16 * 1024;
   begin
      blake2.blake2s_init (sign_ctx'Unchecked_Access);
      loop
         declare
            chunk : constant String := Unix.read_fd (fd, chunk_size);
         begin
            exit when chunk'Length = 0;
            blake2.blake2s_update (sign_ctx'Unchecked_Access, chunk);
         end;
      end loop;
      return blake2.blake2s_final (sign_ctx'Unchecked_Access);
   end checksum_hash_blake2s_file;


   --------------------------------------------------------------------
   --  checksum_hash_blake2b_bulk
   --------------------------------------------------------------------
   function checksum_hash_blake2b_bulk (plain : String) return String
   is
      sign_ctx : aliased blake2.blake2b_state;
   begin
      blake2.blake2b_init (sign_ctx'Unchecked_Access);
      blake2.blake2b_update (sign_ctx'Unchecked_Access, plain);
      return blake2.blake2b_final (sign_ctx'Unchecked_Access);
   end checksum_hash_blake2b_bulk;


   --------------------------------------------------------------------
   --  checksum_hash_blake2s_bulk
   --------------------------------------------------------------------
   function checksum_hash_blake2s_bulk (plain : String) return String
   is
      sign_ctx : aliased blake2.blake2s_state;
   begin
      blake2.blake2s_init (sign_ctx'Unchecked_Access);
      blake2.blake2s_update (sign_ctx'Unchecked_Access, plain);
      return blake2.blake2s_final (sign_ctx'Unchecked_Access);
   end checksum_hash_blake2s_bulk;


   --------------------------------------------------------------------
   --  checksum_hash_file
   --------------------------------------------------------------------
   function checksum_hash_file (fd : Unix.File_Descriptor;
                                checksum_type : A_Checksum_Type) return String is
   begin
      case checksum_type is
         when HASH_TYPE_SHA256_BASE32  |
              HASH_TYPE_SHA256_RAW     |
              HASH_TYPE_SHA256_HEX     => return checksum_hash_sha256_file (fd);
         when HASH_TYPE_BLAKE2_BASE32  |
              HASH_TYPE_BLAKE2_RAW     => return checksum_hash_blake2b_file (fd);
         when HASH_TYPE_BLAKE2S_BASE32 |
              HASH_TYPE_BLAKE2S_RAW    => return checksum_hash_blake2s_file (fd);
         when HASH_TYPE_UNKNOWN        => return "";
      end case;
   end checksum_hash_file;


   --------------------------------------------------------------------
   --  checksum_encode
      --------------------------------------------------------------------
   function checksum_encode (plain : String; checksum_type : A_Checksum_Type) return String is
   begin
      case checksum_type is
         when HASH_TYPE_SHA256_BASE32  |
              HASH_TYPE_BLAKE2_BASE32  |
              HASH_TYPE_BLAKE2S_BASE32 => return checksum_encode_base32 (plain);
         when HASH_TYPE_SHA256_HEX     => return checksum_encode_hex (plain);
         when HASH_TYPE_SHA256_RAW     |
              HASH_TYPE_BLAKE2_RAW     |
              HASH_TYPE_BLAKE2S_RAW    => return plain;
         when HASH_TYPE_UNKNOWN        => return "";
      end case;
   end checksum_encode;


   --------------------------------------------------------------------
   --  checksum_encode_base32
   --------------------------------------------------------------------
   function checksum_encode_base32 (plain : String) return String
   is
      type fullword is mod 2 ** 16;
      subtype bits5 is Natural range 0 .. 4;

      function SR (number : fullword; places : Natural) return fullword;
      function SL (number : fullword; places : Natural) return fullword;
      procedure sendout (x : fullword);

      --  We use here z-base32 encoding described here:
      --  http://philzimmermann.com/docs/human-oriented-base-32-encoding.txt
      b32 : constant String (1 .. 32) := "ybndrfg8ejkmcpqxot1uwisza345h769";

      scenario  : bits5;
      mylast    : Natural := (plain'Length * 8 + 4) / 5;
      result    : String (1 .. mylast);
      index     : Natural := result'First;
      rawbytes  : array (0 .. plain'Length - 1) of fullword;
      x         : fullword;
      x2        : fullword;
      remain    : fullword := 0;
      remaining : Boolean := False;

      function SR (number : fullword; places : Natural) return fullword is
      begin
         return fullword (ITF.Shift_Right (ITF.Unsigned_16 (number), places));
      end SR;

      function SL (number : fullword; places : Natural) return fullword is
      begin
         return fullword (ITF.Shift_Left (ITF.Unsigned_16 (number), places));
      end SL;

      procedure sendout (x : fullword) is
      begin
         result (index) := b32 (b32'First + Integer (x and 16#1F#));
         index := index + 1;
      end sendout;

   begin
      declare
         b : Natural := rawbytes'First;
      begin
         for k in plain'Range loop
            rawbytes (b) := fullword (Character'Pos (plain (k)));
            b := b + 1;
         end loop;
      end;

      for k in rawbytes'Range loop
         scenario := k mod 5;
         case scenario is
            when 0 =>
               --  8 bits of input and 3 to remain
               x := SR (rawbytes (k), 3);
               remain := SL (rawbytes (k) and 2#111#, 2);
               sendout (x);
               remaining := True;
            when 1 =>
               --  11 bits of input, 1 to remain
               x  := remain or SR (rawbytes (k), 6);
               x2 := SR ((rawbytes (k) and 2#00111110#), 1);
               remain := SL (rawbytes (k) and 2#1#, 4);
               sendout (x);
               sendout (x2);
            when 2 =>
               --  9 bits of input, 4 to remain
               x := remain or SR (rawbytes (k), 4);
               remain := SL (rawbytes (k) and 2#1111#, 1);
               sendout (x);
            when 3 =>
               --  12 bits of input, 2 to remain
               x  := remain or SR (rawbytes (k), 7);
               x2 := SR ((rawbytes (k) and 2#01111100#), 2);
               remain := SL (rawbytes (k) and 2#11#, 3);
               sendout (x);
               sendout (x2);
            when 4 =>
               --  10 bits of output, nothing to remain
               x  := remain or SR (rawbytes (k), 5);
               x2 := rawbytes (k) and 2#11111#;
               remain := 0;
               sendout (x);
               sendout (x2);
               remaining := False;
         end case;
      end loop;

      if remaining then
         sendout (remain);
      end if;

      return result;
   end checksum_encode_base32;


   --------------------------------------------------------------------
   --  checksum_encode_hex
   --------------------------------------------------------------------
   function checksum_encode_hex (plain : String) return String
   is
      result : String (1 .. plain'Length * 2);
      index  : Natural := 1;
   begin
      for x in plain'Range loop
         result (index .. index + 1) := Utilities.char2hex (plain (x));
         index := index + 2;
      end loop;

      return result;
   end checksum_encode_hex;


   --------------------------------------------------------------------
   --  checksum_hash_bulk
   --------------------------------------------------------------------
   function checksum_hash_bulk (plain : String;  checksum_type : A_Checksum_Type)
                                return String is
   begin
      case checksum_type is
         when HASH_TYPE_SHA256_BASE32  |
              HASH_TYPE_SHA256_HEX     |
              HASH_TYPE_SHA256_RAW     => return checksum_hash_sha256_bulk (plain);
         when HASH_TYPE_BLAKE2_BASE32  |
              HASH_TYPE_BLAKE2_RAW     => return checksum_hash_blake2b_bulk (plain);
         when HASH_TYPE_BLAKE2S_BASE32 |
              HASH_TYPE_BLAKE2S_RAW    => return checksum_hash_blake2s_bulk (plain);
         when HASH_TYPE_UNKNOWN        => return "";
      end case;
   end checksum_hash_bulk;


   --------------------------------------------------------------------
   --  checksum_fileat
   --------------------------------------------------------------------
   function checksum_fileat
     (rootfd        : Unix.File_Descriptor;
      path          : String;
      checksum_type : A_Checksum_Type)
      return String
   is
      fd : Unix.File_Descriptor;
   begin
      fd := Unix.open_file (dirfd         => rootfd,
                            relative_path => path,
                            flags         => (RDONLY => True, others => False));
      if not Unix.file_connected (fd) then
         Event.emit_errno ("checksum_fileat/open", "rootfd, " & path, Unix.errno);
         return "";
      end if;

      declare
         result : constant String := checksum_fd (fd, checksum_type);
      begin
         if not Unix.close_file (fd) then
            Event.emit_errno ("checksum_fileat/close", "fd", Unix.errno);
         end if;
         return result;
      end;
   end checksum_fileat;


   --------------------------------------------------------------------
   --  checksum_symlink
   --------------------------------------------------------------------
   function checksum_symlink (path : String; checksum_type : A_Checksum_Type) return String
   is
      link_path : String := Unix.readlink (path);
   begin
      if IsBlank (link_path) then
         Event.emit_errno ("checksum_symlink", path, Unix.errno);
         return link_path;
      end if;
      return checksum_symlink_readlink (link_path, checksum_type);
   end checksum_symlink;


   --------------------------------------------------------------------
   --  checksum_symlinkat
   --------------------------------------------------------------------
   function checksum_symlinkat
     (fd            : Unix.File_Descriptor;
      relative_path : String;
      checksum_type : A_Checksum_Type) return String
   is
      link_path : String := Unix.readlink (fd, relative_path);
   begin
      if IsBlank (link_path) then
         Event.emit_errno ("checksum_symlinkat", "fd, " & relative_path, Unix.errno);
         return link_path;
      end if;
      return checksum_symlink_readlink (link_path, checksum_type);
   end checksum_symlinkat;


   --------------------------------------------------------------------
   --  checksum_data
   --------------------------------------------------------------------
   function checksum_data (instr : String;  checksum_type : A_Checksum_Type) return String is
   begin
      if instr'Length = 0 then
         return "";
      end if;

      return checksum_encode
        (plain         => checksum_hash_bulk (instr, checksum_type),
         checksum_type => checksum_type);
   end checksum_data;


   --------------------------------------------------------------------
   --  checksum_symlink_readlink
   --------------------------------------------------------------------
   function checksum_symlink_readlink
     (link_path     : String;
      checksum_type : A_Checksum_Type) return String is
   begin
      return checksum_data (Utilities.relative_path (link_path), checksum_type);
   end checksum_symlink_readlink;


   --------------------------------------------------------------------
   --  checksum_file_get_type
   --------------------------------------------------------------------
   function checksum_file_get_type (cksum : String) return A_Checksum_Type
   is
      --  <hashtype>$<hash>
   begin
      if not contains (cksum, CHECKSUM_SEPARATOR) then
         return HASH_TYPE_UNKNOWN;
      end if;
      return checksum_get_type_helper (specific_field (cksum, 1, CHECKSUM_SEPARATOR));
   end checksum_file_get_type;


   --------------------------------------------------------------------
   --  checksum_get_type
   --------------------------------------------------------------------
   function checksum_get_type (cksum : String) return A_Checksum_Type
   is
      --  <version>$<hashtype>$<hash>
   begin
      if count_char (cksum, CHECKSUM_SEPARATOR (1)) /= 2 then
         return HASH_TYPE_UNKNOWN;
      end if;
      return checksum_get_type_helper (specific_field (cksum, 2, CHECKSUM_SEPARATOR));
   end checksum_get_type;


   --------------------------------------------------------------------
   --  checksum_get_type_helper
   --------------------------------------------------------------------
   function checksum_get_type_helper (frag : String) return A_Checksum_Type is
   begin
      case Integer'Value (frag) is
         when 0 => return HASH_TYPE_SHA256_BASE32;
         when 1 => return HASH_TYPE_SHA256_HEX;
         when 2 => return HASH_TYPE_BLAKE2_BASE32;
         when 3 => return HASH_TYPE_SHA256_RAW;
         when 4 => return HASH_TYPE_BLAKE2_RAW;
         when 5 => return HASH_TYPE_BLAKE2S_BASE32;
         when 6 => return HASH_TYPE_BLAKE2S_RAW;
         when others => return HASH_TYPE_UNKNOWN;
      end case;
   exception
      when others =>
         return HASH_TYPE_UNKNOWN;
   end checksum_get_type_helper;


   --------------------------------------------------------------------
   --  checksum_validate_file
   --------------------------------------------------------------------
   function checksum_validate_file (path : String; sum : String) return Boolean
   is
      checksum_type : A_Checksum_Type;
      sum_txt       : Text;
      newsum_txt    : Text;
      sb            : aliased Unix.struct_stat;
   begin
      checksum_type := checksum_file_get_type (sum);
      case checksum_type is
         when HASH_TYPE_UNKNOWN =>
            checksum_type := HASH_TYPE_SHA256_HEX;
            sum_txt := SUS (sum);
         when others =>
            sum_txt := SUS (specific_field (sum, 2, CHECKSUM_SEPARATOR));
      end case;

      if not Unix.lstat_ok (path, sb'Unchecked_Access) then
         return False;
      end if;

      if Unix.is_link (sb'Unchecked_Access) then
         newsum_txt := SUS (checksum_symlink (path, checksum_type));
      else
         newsum_txt := SUS (checksum_file (path, checksum_type));
      end if;

      return equivalent (newsum_txt, sum_txt);
   end checksum_validate_file;


   --------------------------------------------------------------------
   --  checksum_generate_file
   --------------------------------------------------------------------
   function checksum_generate_file (path : String; checksum_type : A_Checksum_Type)
                                    return String
   is
      sb      : aliased Unix.struct_stat;
      sum_txt : Text;
   begin
      if not Unix.lstat_ok (path, sb'Unchecked_Access) then
         Event.emit_errno ("checksum_generate_file/lstat", path, Unix.errno);
         return "";
      end if;
      if Unix.is_link (sb'Unchecked_Access) then
         sum_txt := SUS (checksum_symlink (path, checksum_type));
      else
         sum_txt := SUS (checksum_file (path, checksum_type));
      end if;

      if IsBlank (sum_txt) then
         return "";
      end if;

      return int2str (A_Checksum_Type'Pos (checksum_type)) & CHECKSUM_SEPARATOR & USS (sum_txt);
   end checksum_generate_file;


   --------------------------------------------------------------------
   --  checksum_size
   --------------------------------------------------------------------
   function checksum_size (checksum_type : A_Checksum_Type) return Natural is
   begin
      case checksum_type is
         when HASH_TYPE_UNKNOWN =>
            return 0;
         when HASH_TYPE_BLAKE2S_BASE32
            | HASH_TYPE_BLAKE2S_RAW =>
            return blake2.blake2s_size;
         when HASH_TYPE_BLAKE2_BASE32
            | HASH_TYPE_BLAKE2_RAW =>
            return blake2.blake2b_size;
         when HASH_TYPE_SHA256_BASE32
            | HASH_TYPE_SHA256_HEX
            | HASH_TYPE_SHA256_RAW =>
            return SSL.sha256_size;
      end case;
   end checksum_size;

end Core.Checksum;
