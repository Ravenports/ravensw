--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


package body blake_3 is

   --------------------------------------------------------------------
   --  b3_update
   --------------------------------------------------------------------
   procedure b3_update (self : blake3_hasher_Access; plain : String)
   is
      buffer : array (plain'Range) of aliased IC.unsigned_char;
   begin
      for x in plain'Range loop
         buffer (x) := IC.unsigned_char (Character'Pos (plain (x)));
      end loop;
      C_b3hasher_update (self, buffer (buffer'First)'Access, plain'Length);
   end b3_update;


   --------------------------------------------------------------------
   --  b3_finalize
   --------------------------------------------------------------------
   function b3_finalize (self : blake3_hasher_Access) return blake3_hash
   is
      result : blake3_hash;
      buffer : array (blake3_hash'Range) of aliased IC.unsigned_char;
   begin
      C_b3hasher_finalize (self, buffer (buffer'First)'Access, BLAKE3_OUT_LEN);
      for x in blake3_hash'Range loop
         result (x) := Character'Val (buffer (x));
      end loop;
      return result;
   end b3_finalize;

end blake_3;
