--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Unchecked_Conversion;

package body Regex is

   --------------------------------------------------------------------
   --  free
   --------------------------------------------------------------------
   procedure free (preg : regex_h.regex_t_Access)
   is
      function convert2void is new Ada.Unchecked_Conversion (Source => regex_h.regex_t_Access,
                                                             Target => free_ptr);
      reg_ptr : free_ptr;
   begin
      reg_ptr := convert2void (preg);
      re_free (reg_ptr);
   end free;

end Regex;
