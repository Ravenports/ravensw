--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Exceptions;
with Core.Checksum;
with Core.Strings;
with Core.Event;
with Core.File_Operations;
with SSL;

use Core.Strings;

package body Core.RSA is

   --------------------------------------------------------------------
   --  deprecated_rsa_verify
   --------------------------------------------------------------------
   function deprecated_rsa_verify
     (key_file  : String;
      signature : String;
      fd        : Unix.File_Descriptor) return Action_Result
   is
      key : String := File_Operations.get_file_contents (key_file);
   begin
      if not Unix.reset_file_for_reading (fd) then
         return RESULT_FATAL;
      end if;

      SSL.SSL_load_error_strings;
      SSL.OpenSSL_add_all_algorithms;

      declare
         sha256 : String := Checksum.checksum_fd (fd, Checksum.HASH_TYPE_SHA256_HEX);
      begin
         if IsBlank (sha256) then
            return RESULT_FATAL;
         end if;
         declare
            my_rsa : SSL.RSA_Access;
            verified : Boolean;
         begin
            my_rsa := SSL.load_rsa_public_key_buf (key);
            if SSL.RSA_empty (my_rsa) then
               Event.emit_error
                  ("deprecated rsa verify - reading public key: " & SSL.get_error_string);
               return RESULT_FATAL;
            end if;
            verified := SSL.RSA_verified (sha256, signature, my_rsa);
            SSL.RSA_free (my_rsa);
            if verified then
               return RESULT_OK;
            else
               return RESULT_FATAL;
            end if;
         end;
      end;
   exception
      when FOP : File_Operations.file_handling =>
         Event.emit_error ("deprecated_rsa_verify, " & Ada.Exceptions.Exception_Message (FOP));
         return RESULT_FATAL;
   end deprecated_rsa_verify;


   --------------------------------------------------------------------
   --  rsa_verify_cert
   --------------------------------------------------------------------
   function rsa_verify_cert
     (key       : String;
      signature : String;
      fd        : Unix.File_Descriptor) return Action_Result
   is
   begin
      if not Unix.reset_file_for_reading (fd) then
         return RESULT_FATAL;
      end if;

      SSL.SSL_load_error_strings;
      SSL.OpenSSL_add_all_algorithms;

      declare
         sha256 : String := Checksum.checksum_fd (fd, Checksum.HASH_TYPE_SHA256_HEX);
      begin
         if IsBlank (sha256) then
            return RESULT_FATAL;
         end if;
         declare
            hash   : String := Checksum.checksum_data (sha256, Checksum.HASH_TYPE_SHA256_RAW);
            my_rsa : SSL.RSA_Access;
            verified : Boolean;
         begin
            my_rsa := SSL.load_rsa_public_key_buf (key);
            if SSL.RSA_empty (my_rsa) then
               Event.emit_error ("verify_cert - reading public key: " & SSL.get_error_string);
               return RESULT_FATAL;
            end if;
            verified := SSL.RSA_cert_verified (hash, signature, my_rsa);
            SSL.RSA_free (my_rsa);
            if verified then
               return RESULT_OK;
            else
               return RESULT_FATAL;
            end if;
         end;
      end;
   end rsa_verify_cert;


end Core.RSA;
