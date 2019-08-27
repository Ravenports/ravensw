--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with System;
with regex_h;

package Regex is

   procedure free (preg : regex_h.regex_t_Access);

private

   subtype free_ptr is System.Address;

   procedure re_free (ptr : free_ptr);
   pragma Import (C, re_free, "free");

end Regex;
