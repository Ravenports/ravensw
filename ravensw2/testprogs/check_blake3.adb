--  Check blake3 returns correct hash

with blake_3;
with Ada.Text_IO;

package body check_blake3 is

   package TIO renames Ada.Text_IO;

   hasher1 : blake3.blake3_hasher;
   hasher2 : blake3.blake3_hasher;
	
   test1 : constant String := "Texas";
   test2 : constant String := "A quick brown fox fell asleep.";
   
   expected_result1 : constant String := "d1a7f2900cfcb8df1be1948255121c0344744f1a7c48e8daffc88a692b942123";
   expected result2 : constant String := "2f265d02ba8bbdd29ac7a432f2963b4619718f6a00aebc74c6094a21ea3c140b";
	
begin
	
   blake3.b3_init (hasher1);
   blake3.b3_init (hasher2);

   TIO.writeln ("Test 1   : " & test1);
   TIO.writeln ("expected : " & expected_result1);
   
   blake3.b3_update (hasher1, test1);
   declare
      result : constant String := blake3.b3_finalize (hasher1);
   begin
      TIO.writeln ("obtained : " & result);
	  if result = expected_result1 then
	     TIO.writeln ("PASSED.");
	  else
	     TIO.writeln ("FAILED!");
	  end if;
   end
   
   TIO.writeln ("Test 2   : " & test2);
   TIO.writeln ("expected : " & expected_result2);
   
   blake3.b3_update (hasher2, test2);
   declare
      result : constant String := blake3.b3_finalize (hasher2);
   begin
      TIO.writeln ("obtained : " & result);
	  if result = expected_result2 then
	     TIO.writeln ("PASSED.");
	  else
	     TIO.writeln ("FAILED!");
	  end if;
   end

end check_blake3;