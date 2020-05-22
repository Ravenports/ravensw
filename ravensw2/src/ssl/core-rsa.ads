--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Unix;

package Core.RSA is

   function deprecated_rsa_verify
     (key       : String;
      signature : String;
      fd        : Unix.File_Descriptor) return Action_Result;

   function rsa_verify_cert
     (key       : String;
      signature : String;
      fd        : Unix.File_Descriptor) return Action_Result;

end Core.RSA;
