--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Interfaces.C.Extensions;

package ssl is

   type SHA256_CTX is private;
   subtype SHA256_hash is String (1 .. 32);

   type SHA256_CTX_Access is access all SHA256_CTX;
   pragma Convention (C, SHA256_CTX_Access);

   procedure sha256_init (ctx : SHA256_CTX_Access);
   pragma Import (C, sha256_init, "SHA256_Init");

   function sha256_final (ctx : SHA256_CTX_Access) return SHA256_hash;

   procedure sha256_update (ctx : SHA256_CTX_Access; plain : String);

private

   package IC  renames Interfaces.C;

   type T_sha256_data  is array (1 .. 64) of IC.unsigned_char;
   type T_sha256_state is array (1 .. 8) of IC.unsigned;

   type SHA256_CTX is
      record
         h      : T_sha256_state;
         Nl     : IC.unsigned;
         Nh     : IC.unsigned;
         data   : T_sha256_data;
         num    : IC.unsigned;
         md_len : IC.unsigned;
      end record;
   pragma Convention (C, SHA256_CTX);

   procedure C_sha256_final (outhash : access IC.unsigned_char; ctx : SHA256_CTX_Access);
   pragma Import (C, C_sha256_final, "SHA256_Final");

   procedure C_sha256_update (ctx  : SHA256_CTX_Access;
                              data : access IC.unsigned_char;
                              len  : IC.size_t);
   pragma Import (C, C_sha256_update, "SHA256_Update");
end ssl;
