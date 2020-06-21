--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Interfaces.C.Extensions;
private with Interfaces.C.Strings;
private with System;

package SSL is

   type SHA256_CTX is private;
   subtype SHA256_hash is String (1 .. 32);

   type SHA256_CTX_Access is access all SHA256_CTX;
   pragma Convention (C, SHA256_CTX_Access);

   procedure sha256_init (ctx : SHA256_CTX_Access);
   pragma Import (C, sha256_init, "SHA256_Init");

   function sha256_size return Natural;
   function sha256_final (ctx : SHA256_CTX_Access) return SHA256_hash;

   procedure sha256_update (ctx : SHA256_CTX_Access; plain : String);

   procedure SSL_load_error_strings;
   procedure OpenSSL_add_all_algorithms;
   procedure OpenSSL_add_all_ciphers;

   type RSA is limited private;
   type RSA_Access is access all RSA;
   pragma Convention (C, RSA_Access);

   --  returns null on failure
   function load_rsa_public_key_buf (cert : String) return RSA_Access;

   function get_error_string return String;

   function RSA_verified (hash, signature : String; RSA_pointer : RSA_Access) return Boolean;
   function RSA_cert_verified (hash, signature : String; RSA_pointer : RSA_Access) return Boolean;

   procedure RSA_free (RSA : RSA_Access);
   pragma Import (C, RSA_free, "RSA_free");

   function RSA_empty (RSA : RSA_Access) return Boolean;

private

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   type T_sha256_data  is array (1 .. 64) of IC.unsigned_char;
   type T_sha256_state is array (1 .. 8) of IC.unsigned;

   type uint64 is mod 2 ** 64;

   type RSA is limited null record;

   type BIO is limited null record;
   type BIO_Access is access all BIO;
   pragma Convention (C, BIO_Access);

   OPENSSL_INIT_LOAD_SSL_STRINGS    : constant uint64 := 16#0020_0000#;
   OPENSSL_INIT_LOAD_CRYPTO_STRINGS : constant uint64 := 16#0000_0002#;
   OPENSSL_INIT_ADD_ALL_CIPHERS     : constant uint64 := 16#0000_0004#;
   OPENSSL_INIT_ADD_ALL_DIGESTS     : constant uint64 := 16#0000_0008#;
   OPENSSL_INIT_LOAD_CONFIG         : constant uint64 := 16#0000_0040#;

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

   function C_OPENSSL_init_ssl (opts : uint64; settings : access IC.int) return IC.int;
   pragma Import (C, C_OPENSSL_init_ssl, "OPENSSL_init_ssl");

   function C_OPENSSL_init_crypto (opts : uint64; settings : access IC.int) return IC.int;
   pragma Import (C, C_OPENSSL_init_crypto, "OPENSSL_init_crypto");

   function C_RSA_verify
     (rsa_type : IC.int;
      m        : access IC.unsigned_char;
      m_length : IC.unsigned;
      sigbuf   : access IC.unsigned_char;
      siglen   : IC.unsigned;
      RSA      : RSA_Access) return IC.int;
   pragma Import (C, C_RSA_verify, "RSA_verify");

   function C_BIO_free (BIO : BIO_Access) return IC.int;
   pragma Import (C, C_BIO_free, "BIO_free");

   function C_BIO_new_mem_buf
     (buf : ICS.char_array_access;
      len : IC.int) return BIO_Access;
   pragma Import (C, C_BIO_new_mem_buf, "BIO_new_mem_buf");

   procedure free_BIO (BIO : BIO_Access);
   function create_new_memory_BIO (buffer : aliased in out IC.char_array) return BIO_Access;

   subtype Void_Ptr is System.Address;

   type cb_pem_password is access function
     (buf    : ICS.char_array_access;
      size   : IC.int;
      rwflag : IC.int;
      u      : Void_Ptr) return IC.int;
   pragma Convention (C, cb_pem_password);

   function PEM_read_bio_RSA_PUBKEY
     (bp : BIO_Access;
      x  : access RSA_Access;
      cb : cb_pem_password;
      u  : Void_Ptr) return RSA_Access;
   pragma Import (C, PEM_read_bio_RSA_PUBKEY, "PEM_read_bio_RSA_PUBKEY");

   function ERR_error_string (e : IC.unsigned_long; buf : access IC.char) return ICS.chars_ptr;
   pragma Import (C, ERR_error_string, "ERR_error_string");

   function ERR_get_error return IC.unsigned_long;
   pragma Import (C, ERR_get_error, "ERR_get_error");

end SSL;
