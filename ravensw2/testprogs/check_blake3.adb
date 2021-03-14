--  Check blake3 returns correct hash

with interfaces;
with blake_3;
with Ada.Text_IO;

procedure check_blake3 is

   package TIO renames Ada.Text_IO;
   package INT renames Interfaces;

   subtype hexrep is String (1 .. 2);

   hasher1 : aliased blake_3.blake3_hasher;
   hasher2 : aliased blake_3.blake3_hasher;
   hasher3 : aliased blake_3.blake3_hasher;

   test1 : constant String := "Texas";
   test2 : constant String := "A quick brown fox fell asleep.";
   test3 : constant String := "";

   expected_result1 : constant String := "f4b213a9769f4fbe4f49682c1a25186e16509a8960ecbba31967172c3d9f9d90";
   expected_result2 : constant String := "8aa63858237fb0dfb04f6eba622a3b0d89bfef7be50c73b41bb54dff2221c4b8";
   expected_result3 : constant String := "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262";

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

   function print_hash (myhash : String) return String
   is
     result : String (1 .. myhash'Length * 2);
     index  : positive := 1;
   begin
      for x in myhash'Range loop
         result (index .. index + 1) := char2hex (myhash (x));
         index := index + 2;
      end loop;
      return result;
   end print_hash;

begin

   blake_3.b3_init (hasher1'Unchecked_Access);
   blake_3.b3_init (hasher2'Unchecked_Access);
   blake_3.b3_init (hasher3'Unchecked_Access);

   TIO.Put_Line ("Test 1   : " & test1);
   TIO.Put_Line ("expected : " & expected_result1);

   blake_3.b3_update (hasher1'Unchecked_Access, test1);
   declare
      result : constant String := print_hash (blake_3.b3_finalize (hasher1'Unchecked_Access));
   begin
      TIO.Put_Line ("obtained : " & result);
	  if result = expected_result1 then
	     TIO.Put_Line ("PASSED.");
	  else
	     TIO.Put_Line ("FAILED!");
	  end if;
   end;

   TIO.Put_Line ("Test 2   : " & test2);
   TIO.Put_Line ("expected : " & expected_result2);

   blake_3.b3_update (hasher2'Unchecked_Access, test2);
   declare
      result : constant String := print_hash (blake_3.b3_finalize (hasher2'Unchecked_Access));
   begin
      TIO.Put_Line ("obtained : " & result);
	  if result = expected_result2 then
	     TIO.Put_Line ("PASSED.");
	  else
	     TIO.Put_Line ("FAILED!");
	  end if;
   end;

   TIO.Put_Line ("Test 3   : " & test3);
   TIO.Put_Line ("expected : " & expected_result3);

   blake_3.b3_update (hasher2'Unchecked_Access, test3);
   declare
      result : constant String := print_hash (blake_3.b3_finalize (hasher3'Unchecked_Access));
   begin
      TIO.Put_Line ("obtained : " & result);
	  if result = expected_result3 then
	     TIO.Put_Line ("PASSED.");
	  else
	     TIO.Put_Line ("FAILED!");
	  end if;
   end;

end check_blake3;
