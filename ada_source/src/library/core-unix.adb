--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Unix is

   --------------------------------------------------------------------
   --  strerror
   --------------------------------------------------------------------
   function strerror (errno : Integer) return String
   is
      use type IC.Strings.chars_ptr;

      C_Msg : IC.Strings.chars_ptr;
   begin
      C_Msg := C_Strerror (IC.int (errno));

      if C_Msg = IC.Strings.Null_Ptr then
         return "Unknown system error";
      else
         return IC.Strings.Value (C_Msg);
      end if;

   end strerror;



end Core.Unix;
