--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


package body blake2 is

   --------------------------------------------------------------------
   --  blake2b_init
   --------------------------------------------------------------------
   procedure blake2b_init (b2b : blake2b_state_Access)
   is
      res : IC.int;
   begin
      res := C_blake2b_init (b2b, BLAKE2B_OUTBYTES);
   end blake2b_init;


   --------------------------------------------------------------------
   --  blake2s_init
   --------------------------------------------------------------------
   procedure blake2s_init (b2b : blake2s_state_Access)
   is
      res : IC.int;
   begin
      res := C_blake2s_init (b2b, BLAKE2S_OUTBYTES);
   end blake2s_init;


   --------------------------------------------------------------------
   --  blake2b_update
   --------------------------------------------------------------------
   procedure blake2b_update (b2b : blake2b_state_Access; plain : String)
   is
      buffer : array (plain'Range) of aliased IC.unsigned_char;
      res    : IC.int;
   begin
      for x in plain'Range loop
         buffer (x) := IC.unsigned_char (Character'Pos (plain (x)));
      end loop;
      res := C_blake2b_update (b2b, buffer (buffer'First)'Access, plain'Length);
   end blake2b_update;


   --------------------------------------------------------------------
   --  blake2s_update
   --------------------------------------------------------------------
   procedure blake2s_update (b2b : blake2s_state_Access; plain : String)
   is
      buffer : array (plain'Range) of aliased IC.unsigned_char;
      res    : IC.int;
   begin
      for x in plain'Range loop
         buffer (x) := IC.unsigned_char (Character'Pos (plain (x)));
      end loop;
      res := C_blake2s_update (b2b, buffer (buffer'First)'Access, plain'Length);
   end blake2s_update;


   --------------------------------------------------------------------
   --  blake2b_final
   --------------------------------------------------------------------
   function blake2b_final (b2b : blake2b_state_Access) return blake2b_hash
   is
      result : blake2b_hash;
      buffer : array (blake2b_hash'Range) of aliased IC.unsigned_char;
      res    : IC.int;
   begin
      res := C_blake2b_final (b2b, buffer (buffer'First)'Access, BLAKE2B_OUTBYTES);
      for x in blake2b_hash'Range loop
         result (x) := Character'Val (buffer (x));
      end loop;
      return result;
   end blake2b_final;


   --------------------------------------------------------------------
   --  blake2s_final
   --------------------------------------------------------------------
   function blake2s_final (b2b : blake2s_state_Access) return blake2s_hash
   is
      result : blake2s_hash;
      buffer : array (blake2s_hash'Range) of aliased IC.unsigned_char;
      res    : IC.int;
   begin
      res := C_blake2s_final (b2b, buffer (buffer'First)'Access, BLAKE2S_OUTBYTES);
      for x in blake2s_hash'Range loop
         result (x) := Character'Val (buffer (x));
      end loop;
      return result;
   end blake2s_final;

end blake2;
