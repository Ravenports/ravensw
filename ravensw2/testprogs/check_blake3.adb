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

   test1 : constant String := "Texas";
   test2 : constant String := "A quick brown fox fell asleep.";

   expected_result1 : constant String := "d1a7f2900cfcb8df1be1948255121c0344744f1a7c48e8daffc88a692b942123";
   expected_result2 : constant String := "2f265d02ba8bbdd29ac7a432f2963b4619718f6a00aebc74c6094a21ea3c140b";

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

end check_blake3;
