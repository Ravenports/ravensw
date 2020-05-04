--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body ssl is

   --------------------------------------------------------------------
   --  sha256_final
   --------------------------------------------------------------------
   function sha256_final (ctx : SHA256_CTX_Access) return SHA256_hash
   is
      result : SHA256_hash;
      buffer : array (SHA256_hash'Range) of aliased IC.unsigned_char;
   begin
      C_sha256_final (buffer (buffer'First)'Access, ctx);
      for x in SHA256_hash'Range loop
         result (x) := Character'Val (buffer (x));
      end loop;
      return result;
   end sha256_final;


   --------------------------------------------------------------------
   --  sha256_final
   --------------------------------------------------------------------
   procedure sha256_update (ctx : SHA256_CTX_Access; plain : String)
   is
      buffer : array (plain'Range) of aliased IC.unsigned_char;
   begin
      for x in plain'Range loop
         buffer (x) := IC.unsigned_char (Character'Pos (plain (x)));
      end loop;
      C_sha256_update (ctx, buffer (buffer'First)'Access, plain'Length);
   end sha256_update;

end ssl;
