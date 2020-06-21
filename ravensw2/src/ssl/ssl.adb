--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body SSL is

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
   --  sha256_size
   --------------------------------------------------------------------
   function sha256_size return Natural is
   begin
      return SHA256_hash'Length;
   end sha256_size;


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


   --------------------------------------------------------------------
   --  SSL_load_error_strings
   --------------------------------------------------------------------
   procedure SSL_load_error_strings
   is
      flags : uint64 := OPENSSL_INIT_LOAD_SSL_STRINGS and OPENSSL_INIT_LOAD_CRYPTO_STRINGS;
      res   : IC.int;
   begin
      res := C_OPENSSL_init_ssl (flags, null);
   end SSL_load_error_strings;


   --------------------------------------------------------------------
   --  OpenSSL_add_all_algorithms
   --------------------------------------------------------------------
   procedure OpenSSL_add_all_algorithms
   is
      flags : uint64 := OPENSSL_INIT_ADD_ALL_CIPHERS
        and OPENSSL_INIT_ADD_ALL_DIGESTS
        and OPENSSL_INIT_LOAD_CONFIG;
      res   : IC.int;
   begin
      res := C_OPENSSL_init_crypto (flags, null);
   end OpenSSL_add_all_algorithms;


   --------------------------------------------------------------------
   --  OpenSSL_add_all_ciphers
   --------------------------------------------------------------------
   procedure OpenSSL_add_all_ciphers
   is
      flags : uint64 := OPENSSL_INIT_ADD_ALL_CIPHERS;
      res   : IC.int;
   begin
      res := C_OPENSSL_init_crypto (flags, null);
   end OpenSSL_add_all_ciphers;


   --------------------------------------------------------------------
   --  free_BIO
   --------------------------------------------------------------------
   procedure free_BIO (BIO : BIO_Access)
   is
      res : IC.int;
   begin
      res := C_BIO_free (BIO);
   end free_BIO;


   --------------------------------------------------------------------
   --  create_new_memory_BIO
   --------------------------------------------------------------------
   function create_new_memory_BIO (buffer : aliased in out IC.char_array) return BIO_Access
   is
      res   : BIO_Access;
      c_len : IC.int := IC.int (buffer'Length);
   begin
      res := C_BIO_new_mem_buf (buffer'Access, c_len);

      return res;
   end create_new_memory_BIO;


   --------------------------------------------------------------------
   --  get_error_string
   --------------------------------------------------------------------
   function get_error_string return String
   is
      error : ICS.chars_ptr;
      buffer : aliased IC.char_array (1 .. 1024);
   begin
      error := ERR_error_string (ERR_get_error, buffer (buffer'First)'Access);
      return ICS.Value (error);
   end get_error_string;


   --------------------------------------------------------------------
   --  load_rsa_public_key_buf
   --------------------------------------------------------------------
   function load_rsa_public_key_buf (cert : String) return RSA_Access
   is
      use type IC.size_t;

      c_buffer : aliased IC.char_array := (1 .. cert'Length => ' ');
      index    : IC.size_t := c_buffer'First;
      my_bio   : BIO_Access;
      result   : RSA_Access;
   begin
      for X in cert'Range loop
         c_buffer (index) := IC.char (cert (X));
         index := index + 1;
      end loop;
      my_bio := create_new_memory_BIO (c_buffer);

      result := PEM_read_bio_RSA_PUBKEY (bp => my_bio,
                                         x  => null,
                                         cb => null,
                                         u  => System.Null_Address);
      free_BIO (my_bio);
      return result;
   end load_rsa_public_key_buf;


   --------------------------------------------------------------------
   --  RSA_verify
   --------------------------------------------------------------------
   function RSA_verified (hash, signature : String; RSA_pointer : RSA_Access) return Boolean
   is
      use type IC.int;

      --
      --  * Here are dragons:
      --  * 1) rsa_verify is NOT rsa_verify_cert
      --  * 2) siglen must be reduced by one to support this legacy method
      --  *
      --  * by @bdrewery

      NID_sha1 : constant IC.int := 64;
      zerochar : constant IC.unsigned_char := IC.unsigned_char'First;
      res      : IC.int;
      m_length : Natural := Natural (hash'Length);
      siglen   : Natural := Natural (signature'Length - 1);
      m        : array (1 .. m_length) of aliased IC.unsigned_char := (others => zerochar);
      sigbuf   : array (1 .. siglen) of aliased IC.unsigned_char := (others => zerochar);
      index    : Natural;
   begin
      index := hash'First;
      for x in m'Range loop
         m (x) := IC.unsigned_char (Character'Pos (hash (index)));
         index := index + 1;
      end loop;

      index := signature'First;
      for x in sigbuf'Range loop
         sigbuf (x) := IC.unsigned_char (Character'Pos (signature (index)));
         index := index + 1;
      end loop;

      res := C_RSA_verify (rsa_type => NID_sha1,
                           m        => m (m'First)'Access,
                           m_length => IC.unsigned (m_length),
                           sigbuf   => sigbuf (sigbuf'First)'Access,
                           siglen   => IC.unsigned (siglen),
                           RSA      => RSA_pointer);
      return (res = IC.int (1));
   end RSA_verified;


   --------------------------------------------------------------------
   --  RSA_cert_verified
   --------------------------------------------------------------------
   function RSA_cert_verified (hash, signature : String; RSA_pointer : RSA_Access) return Boolean
   is
      use type IC.int;

      NID_sha256 : constant IC.int := 672;
      zerochar : constant IC.unsigned_char := IC.unsigned_char'First;
      res      : IC.int;
      m_length : Natural := Natural (hash'Length);
      siglen   : Natural := Natural (signature'Length);
      m        : array (1 .. m_length) of aliased IC.unsigned_char := (others => zerochar);
      sigbuf   : array (1 .. siglen) of aliased IC.unsigned_char := (others => zerochar);
      index    : Natural;
   begin
      index := hash'First;
      for x in 1 .. m_length loop
         m (x) := IC.unsigned_char (Character'Pos (hash (index)));
         index := index + 1;
      end loop;

      index := signature'First;
      for x in 1 .. siglen loop
         sigbuf (x) := IC.unsigned_char (Character'Pos (signature (index)));
         index := index + 1;
      end loop;
      res := C_RSA_verify (rsa_type => NID_sha256,
                           m        => m (m'First)'Access,
                           m_length => IC.unsigned (m_length),
                           sigbuf   => sigbuf (sigbuf'First)'Access,
                           siglen   => IC.unsigned (siglen),
                           RSA      => RSA_pointer);
      return (res = IC.int (1));
   end RSA_cert_verified;


   --------------------------------------------------------------------
   --  RSA_empty
   --------------------------------------------------------------------
   function RSA_empty (RSA : RSA_Access) return Boolean is
   begin
      return (RSA = null);
   end RSA_empty;


end SSL;
