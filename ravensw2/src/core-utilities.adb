--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Numerics.Discrete_Random;
with Interfaces;
with System;

with Core.Strings;
with Core.Config;
with Core.Event;
with Core.Unix;

use Core.Strings;

package body Core.Utilities is

   package INT renames Interfaces;

   --------------------------------------------------------------------
   --  pkg_absolutepath
   --------------------------------------------------------------------
   function pkg_absolutepath (input_path : String; fromroot : Boolean) return String
   is
      dest : Text;
      slash : constant String := "/";
   begin
      if input_path'Length = 0 then
         return slash;
      else
         if input_path (input_path'First) = '/' then
            if input_path'Length = 1 then
               return slash;
            end if;
         else
            --  We have a relative path here
            if not fromroot then
               dest := SUS (Unix.get_current_working_directory);
               if IsBlank (dest) then  --  problem
                  return "";
               end if;
            end if;
         end if;
      end if;

      declare
         index : Natural := input_path'First;
         ND    : constant Natural := input_path'Last;
         slash_present : Boolean;
         fragment  : Text;
         fraglen   : Natural;
         skip_rest : Boolean;
      begin
         loop
            skip_rest := False;
            slash_present := contains (input_path (index .. ND), slash);
            if slash_present then
               fragment := SUS (part_1 (input_path (index .. ND), slash));
            else
               fragment := SUS (input_path (index .. ND));
            end if;
            fraglen := SU.Length (fragment);

            --  check for special cases "", "." and ".."
            if fraglen = 0 then --  shouldn't happen (?)
               skip_rest := True;
            elsif fraglen = 1 and then input_path (index) = '.' then
               skip_rest := True;
            elsif fraglen = 2 and then input_path (index .. index + 1) = ".." then
               --  Remove entry from destination, we're drilling back up
               dest := SUS (head (USS (dest), slash));
               skip_rest := True;
            end if;
            index := index + fraglen + 1;

            if not skip_rest then
               SU.Append (dest, slash);
               SU.Append (dest, fragment);
            end if;
            exit when not slash_present;
         end loop;
      end;

      return USS (dest);

   end pkg_absolutepath;


   --------------------------------------------------------------------
   --  char2hex
   --------------------------------------------------------------------
   function char2hex (quattro : Character) return hexrep
   is
      type halfbyte is mod 2 ** 4;
      type fullbyte is mod 2 ** 8;
      function halfbyte_to_hex (value : halfbyte) return Character;

      std_byte  : INT.Unsigned_8;
      work_4bit : halfbyte;
      result    : hexrep;

      function halfbyte_to_hex (value : halfbyte) return Character
      is
         zero     : constant Natural := Character'Pos ('0');
         alpham10 : constant Natural := Character'Pos ('a') - 10;
      begin
         case value is
            when 0 .. 9 => return Character'Val (zero + Natural (value));
            when others => return Character'Val (alpham10 + Natural (value));
         end case;
      end halfbyte_to_hex;

   begin
      std_byte   := INT.Unsigned_8 (Character'Pos (quattro));
      work_4bit  := halfbyte (INT.Shift_Right (std_byte, 4));
      result (1) := halfbyte_to_hex (work_4bit);

      work_4bit  := halfbyte (fullbyte (Character'Pos (quattro)) and 2#1111#);
      result (2) := halfbyte_to_hex (work_4bit);

      return result;
   end char2hex;


   --------------------------------------------------------------------
   --  relative_path
   --------------------------------------------------------------------
   function relative_path (input_path : String) return String is
   begin
      if input_path (input_path'First) = '/' then
         if input_path'Length = 1 then
            return "";
         else
            return input_path (input_path'First + 1 .. input_path'Last);
         end if;
      else
         return input_path;
      end if;
   end relative_path;


   --------------------------------------------------------------------
   --  hex2char
   --------------------------------------------------------------------
   function hex2char (hex : hexrep) return Character
   is
      value : Natural;
      V10   : Character := hex (hex'First);
      V01   : Character := hex (hex'Last);
   begin
      --  16ths place
      case V10 is
         when '0' .. '9' => value := (Character'Pos (V10) - Character'Pos ('0')) * 16;
         when 'a' .. 'f' => value := (Character'Pos (V10) - Character'Pos ('a') + 10) * 16;
         when 'A' .. 'F' => value := (Character'Pos (V10) - Character'Pos ('A') + 10) * 16;
         when others => return Character'Val (0);
      end case;

      --  1s place
      case V01 is
         when '0' .. '9' => value := value + (Character'Pos (V01) - Character'Pos ('0'));
         when 'a' .. 'f' => value := value + (Character'Pos (V01) - Character'Pos ('a') + 10);
         when 'A' .. 'F' => value := value + (Character'Pos (V01) - Character'Pos ('A') + 10);
         when others => return Character'Val (0);
      end case;

      return Character'Val (value);
   end hex2char;


   --------------------------------------------------------------------
   --  format_bytes_SI
   --------------------------------------------------------------------
   function format_bytes_SI (bytes : int64) return String
   is
      subtype suffix is String (1 .. 2);
      type Prefix_Group is (B, kB, MB, GB, TB, PB);
      type SI_Set is array (Prefix_Group) of suffix;
      subtype Display_Range is int64 range 0 .. 9_999;

      function display_one_point (value : Display_Range) return String;

      abs_bytes : constant int64 := abs (bytes);
      roundup   : int64;
      adj_bytes : Display_Range;
      suffixes  : constant SI_Set := ("B ", "kB", "MB", "GB", "TB", "PB");

      function display_one_point (value : Display_Range) return String
      is
         L_side    : constant Natural := Natural (value) / 10;
         R_side    : constant Natural := Natural (value) - (L_side * 10);
      begin
         return int2str (L_side) & "." & int2str (R_side);
      end display_one_point;

   begin
      --  less than      10_000: display as "____XB "
      --  less than     999_950: display as "___.XkB" (10.0 .. 999.9)kB
      --  less than 999_950_000: display as "___.XMB" ( 1.0 .. 999.9)MB
      if abs_bytes < 10_000 then
         return pad_left (int2str (Integer (abs_bytes)), 5) & suffixes (B);
      elsif abs_bytes < 999_950 then
         roundup   := 50;
         adj_bytes := (abs_bytes + roundup / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (kB);
      elsif abs_bytes < 999_950_000 then
         roundup   := 50_000;
         adj_bytes := (abs_bytes + roundup / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (MB);
      elsif abs_bytes < 999_950_000_000 then
         roundup   := 50_000_000;
         adj_bytes := (abs_bytes + roundup / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (GB);
      elsif abs_bytes < 999_950_000_000_000 then
         roundup   := 50_000_000_000;
         adj_bytes := (abs_bytes + roundup / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (TB);
      else
         roundup   := 50_000_000_000_000;
         adj_bytes := (abs_bytes + roundup / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (PB);
      end if;
   end format_bytes_SI;


   --------------------------------------------------------------------
   --  format_bytes_IEC
   --------------------------------------------------------------------
   function format_bytes_IEC (bytes : int64) return String
   is
      subtype suffix is String (1 .. 3);
      type Prefix_Group is (B, KiB, MiB, GiB, TiB, PiB);
      type IEC_Set is array (Prefix_Group) of suffix;
      subtype Display_Range is int64 range 0 .. 9_999;

      function display_one_point (value : Display_Range) return String;

      abs_bytes : constant int64 := abs (bytes);
      roundup   : int64;
      adj_bytes : Display_Range;
      suffixes  : constant IEC_Set := ("B  ", "KiB", "MiB", "GiB", "TiB", "PiB");

      function display_one_point (value : Display_Range) return String
      is
         L_side    : constant Natural := Natural (value) / 10;
         R_side    : constant Natural := Natural (value) - (L_side * 10);
      begin
         return int2str (L_side) & "." & int2str (R_side);
      end display_one_point;

   begin
      --  less than        10_240    : display as "____XB "
      --  less than     1_024_000 - r: display as "___.XKiB"  (10.0 .. 999.9) KiB
      --  less than 1,048,576,000 - r: display as "___.XMiB"  ( 1.0 .. 999.9) MiB
      if abs_bytes < 10_240 then
         return pad_left (int2str (Integer (abs_bytes)), 5) & suffixes (B);
      elsif abs_bytes < 1_023_949 then
         roundup   := 512;
         adj_bytes := Display_Range ((abs_bytes * 10 + roundup) / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (KiB);
      elsif abs_bytes < 1_048_523_572 then
         roundup   := 524_288;
         adj_bytes := Display_Range ((abs_bytes * 10 + roundup) / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (MiB);
      elsif abs_bytes < 1_073_688_136_909 then
         roundup   := 536_870_912;
         adj_bytes := Display_Range ((abs_bytes * 10 + roundup) / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (GiB);
      elsif abs_bytes < 1_099_456_652_194_611 then
         roundup   := 549_755_813_888;
         adj_bytes := Display_Range ((abs_bytes * 10 + roundup) / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (TiB);
      else
         roundup   := 562_949_953_421_312;
         adj_bytes := Display_Range ((abs_bytes * 10 + roundup) / (roundup * 2));
         return pad_left (display_one_point (adj_bytes), 5) & suffixes (PiB);
      end if;
   end format_bytes_IEC;


   --------------------------------------------------------------------
   --  random_characters
   --------------------------------------------------------------------
   function random_characters (number : Positive := 6) return String
   is
      type Random_Range is new Integer range 0 .. 61;
      package Rand_Int is new ada.numerics.discrete_random (Random_Range);
      gen : Rand_Int.Generator;
      num : Random_Range;

      result : String (1 .. number);
   begin
      Rand_Int.Reset (gen);
      for x in result'Range loop
         num := Rand_Int.Random (gen);
         case num is
            when  0 ..  9 => result (x) := Character'Val (Character'Pos ('0') + num);
            when 10 .. 35 => result (x) := Character'Val (Character'Pos ('A') + num - 10);
            when 36 .. 61 => result (x) := Character'Val (Character'Pos ('a') + num - 36);
         end case;
      end loop;
      return result;
   end random_characters;


   --------------------------------------------------------------------
   --  conv2int (64-bit version signed)
   --------------------------------------------------------------------
   function conv2int (str : bytes8) return int64
   is
      --  input: [A][B][C][D][E][F][G][H]
      --  Little endian : H<<56 + G<<48 + F<<40 + E<<32 + D<<24 + C<<16 + B<<8 + A
      --  Big endian    : A<<56 + B<<48 + C<<40 + D<<32 + E<<24 + F<<16 + G<<8 + H

      A : INT.Unsigned_64 := INT.Unsigned_64 (Character'Pos (str (str'First)));
      B : INT.Unsigned_64 := INT.Unsigned_64 (Character'Pos (str (str'First + 1)));
      C : INT.Unsigned_64 := INT.Unsigned_64 (Character'Pos (str (str'First + 2)));
      D : INT.Unsigned_64 := INT.Unsigned_64 (Character'Pos (str (str'First + 3)));
      E : INT.Unsigned_64 := INT.Unsigned_64 (Character'Pos (str (str'First + 4)));
      F : INT.Unsigned_64 := INT.Unsigned_64 (Character'Pos (str (str'First + 5)));
      G : INT.Unsigned_64 := INT.Unsigned_64 (Character'Pos (str (str'First + 6)));
      H : INT.Unsigned_64 := INT.Unsigned_64 (Character'Pos (str (str'First + 7)));

      use type INT.Unsigned_64;
      use type System.Bit_Order;

      Little_Endian : constant Boolean := System.Default_Bit_Order = System.Low_Order_First;
      result : INT.Unsigned_64;

   begin
      if Little_Endian then
         result :=   (INT.Shift_Left (H, 56) +
                      INT.Shift_Left (G, 48) +
                      INT.Shift_Left (F, 40) +
                      INT.Shift_Left (E, 32) +
                      INT.Shift_Left (D, 24) +
                      INT.Shift_Left (C, 16) +
                      INT.Shift_Left (B, 8) +
                      A);
      else
         result :=   (INT.Shift_Left (A, 56) +
                      INT.Shift_Left (B, 48) +
                      INT.Shift_Left (C, 40) +
                      INT.Shift_Left (D, 32) +
                      INT.Shift_Left (E, 24) +
                      INT.Shift_Left (F, 16) +
                      INT.Shift_Left (G, 8) +
                      H);
      end if;
      if result < INT.Unsigned_64 (int64'Last) then
         return int64 (result);
      else
         declare
            ones : constant INT.Unsigned_64 := 16#FFFF_FFFF#;
            com2 : INT.Unsigned_64;
         begin
            com2 := (result xor ones) + 1;
            return -1 * int64 (com2);
         end;
      end if;
   end conv2int;


   --------------------------------------------------------------------
   --  conv2int (32-bit version unsigned)
   --------------------------------------------------------------------
   function conv2int (str : bytes4) return uint32
   is
      --  input: [A][B][C][D]
      --  Little endian : D<<24 + C<<16 + B<<8 + A
      --  Big endian    : A<<24 + B<<16 + C<<8 + D

      A : INT.Unsigned_32 := INT.Unsigned_32 (Character'Pos (str (str'First)));
      B : INT.Unsigned_32 := INT.Unsigned_32 (Character'Pos (str (str'First + 1)));
      C : INT.Unsigned_32 := INT.Unsigned_32 (Character'Pos (str (str'First + 2)));
      D : INT.Unsigned_32 := INT.Unsigned_32 (Character'Pos (str (str'First + 3)));

      use type INT.Unsigned_32;
      use type System.Bit_Order;

      Little_Endian : constant Boolean := System.Default_Bit_Order = System.Low_Order_First;
   begin
      if Little_Endian then
         return uint32 (INT.Shift_Left (D, 24) +
                        INT.Shift_Left (C, 16) +
                        INT.Shift_Left (B, 8) +
                        A);
      else
         return uint32 (INT.Shift_Left (A, 24) +
                        INT.Shift_Left (B, 16) +
                        INT.Shift_Left (C, 8) +
                        D);
      end if;
   end conv2int;


   --------------------------------------------------------------------
   --  is_valid_abi
   --------------------------------------------------------------------
   function is_valid_abi (arch : String; show_errors : Boolean) return Boolean
   is
      myarch : String := Config.configuration_value (Config.abi);
   begin
      if not Unix.filename_match (arch, myarch) then
         if arch /= myarch then
            if show_errors then
               Event.emit_error ("wrong architecture: " & arch & " instead of " & myarch);
            end if;
            return False;
         end if;
      end if;
      return True;
   end is_valid_abi;


   --------------------------------------------------------------------
   --  bourne_shell
   --------------------------------------------------------------------
   function bourne_shell return String is
   begin
      case platform is
         when solaris
            | omnios  => return "/usr/xpg4/bin/sh";
         when freebsd
            | dragonfly
            | netbsd
            | openbsd
            | linux
            | macos
            | generic_unix => return "/bin/sh";
      end case;
   end bourne_shell;

end Core.Utilities;
