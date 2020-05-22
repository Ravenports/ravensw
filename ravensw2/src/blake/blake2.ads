--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Interfaces.C.Extensions;

package blake2 is

   type blake2b_state is private;
   type blake2b_state_Access is access all blake2b_state;
   pragma Convention (C, blake2b_state_Access);

   type blake2s_state is private;
   type blake2s_state_Access is access all blake2s_state;
   pragma Convention (C, blake2s_state_Access);

   subtype blake2b_hash is String (1 .. 64);
   subtype blake2s_hash is String (1 .. 32);

   procedure blake2b_init (b2b : blake2b_state_Access);
   procedure blake2s_init (b2b : blake2s_state_Access);

   procedure blake2b_update (b2b : blake2b_state_Access; plain : String);
   procedure blake2s_update (b2b : blake2s_state_Access; plain : String);

   function blake2b_final (b2b : blake2b_state_Access) return blake2b_hash;
   function blake2s_final (b2b : blake2s_state_Access) return blake2s_hash;

   function blake2b_size return Natural;
   function blake2s_size return Natural;

private

   package IC renames Interfaces.C;

   BLAKE2B_BLOCKBYTES   : constant IC.size_t := 128;
   BLAKE2B_BLOCKBYTESx2 : constant IC.size_t := 256;
   BLAKE2B_OUTBYTES     : constant IC.size_t := 64;

   BLAKE2S_BLOCKBYTES   : constant IC.size_t := 64;
   BLAKE2S_BLOCKBYTESx2 : constant IC.size_t := 128;
   BLAKE2S_OUTBYTES     : constant IC.size_t := 32;

   type uint32 is mod 2 ** 32;
   type uint64 is mod 2 ** 64;
   type blake2b_tf  is array (1 .. 2) of uint64;
   type blake2b_h   is array (1 .. 8) of uint64;
   type blake2b_buf is array (1 .. BLAKE2B_BLOCKBYTESx2) of IC.unsigned_char;
   type blake2b_state is
      record
         h         : blake2b_h;
         t         : blake2b_tf;
         f         : blake2b_tf;
         buf       : blake2b_buf;
         buflen    : uint32;
         outlen    : IC.unsigned_char;
         last_node : IC.unsigned_char;
      end record;

   type blake2s_tf  is array (1 .. 2) of uint32;
   type blake2s_h   is array (1 .. 8) of uint32;
   type blake2s_buf is array (1 .. BLAKE2S_BLOCKBYTESx2) of IC.unsigned_char;

   type blake2s_state is
      record
         h         : blake2s_h;
         t         : blake2s_tf;
         f         : blake2s_tf;
         buf       : blake2s_buf;
         buflen    : uint32;
         outlen    : IC.unsigned_char;
         last_node : IC.unsigned_char;
      end record;

   function C_blake2b_init (S : blake2b_state_Access; outlen : IC.size_t) return IC.int;
   pragma Import (C, C_blake2b_init, "blake2b_init");

   function C_blake2b_update (S : blake2b_state_Access;
                              data : access IC.unsigned_char;
                              len  : IC.size_t) return IC.int;
   pragma Import (C, C_blake2b_update, "blake2b_update");

   function C_blake2b_final (S : blake2b_state_Access;
                             outhash : access IC.unsigned_char;
                             outlen  : IC.size_t) return IC.int;
   pragma Import (C, C_blake2b_final, "blake2b_final");

   function C_blake2s_init (S : blake2s_state_Access; outlen : IC.size_t) return IC.int;
   pragma Import (C, C_blake2s_init, "blake2s_init");

   function C_blake2s_update (S : blake2s_state_Access;
                              data : access IC.unsigned_char;
                              len  : IC.size_t) return IC.int;
   pragma Import (C, C_blake2s_update, "blake2s_update");

   function C_blake2s_final (S : blake2s_state_Access;
                             outhash : access IC.unsigned_char;
                             outlen  : IC.size_t) return IC.int;
   pragma Import (C, C_blake2s_final, "blake2s_final");

end blake2;
