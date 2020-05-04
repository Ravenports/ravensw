--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with System;
with Interfaces.C.Strings;

package regex_h is

   pragma Preelaborate;

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   REG_EXTENDED : constant IC.int := IC.int (1);
   REG_ICASE    : constant IC.int := IC.int (2);
   REG_NEWLINE  : constant IC.int := IC.int (4);
   REG_NOSUB    : constant IC.int := IC.int (8);

   REG_OK       : constant IC.int := IC.int (0);
   REG_NOMATCH  : constant IC.int := IC.int (1);

   type regex_t is
      record
         re_magic : IC.int;
         re_nsub  : IC.size_t;
         re_endp  : System.Address;
         value    : System.Address;
      end record;

   subtype regoff_t is IC.int;

   type regmatch_t is
      record
         rm_so : regoff_t;
         rm_eo : regoff_t;
      end record;

   type regmatch_t_Access is access all regmatch_t;
   pragma Convention (C, regmatch_t_Access);

   type regex_t_Access is access all regex_t;
   pragma Convention (C, regex_t_Access);

   function regcomp (preg   : regex_t_Access;
                     regex  : ICS.chars_ptr;
                     cflags : IC.int) return IC.int;
   pragma Import (C, regcomp);

   procedure regfree (preg : regex_t_Access);
   pragma Import (C, regfree);

   function regexec (preg   : regex_t_Access;
                     regex  : ICS.chars_ptr;
                     nmatch : IC.size_t;
                     pmatch : regmatch_t_Access;
                     eflags : IC.int) return IC.int;
   pragma Import (C, regexec);
end regex_h;
