--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Unbounded;

package Core is

   package SU renames Ada.Strings.Unbounded;

   subtype Text is SU.Unbounded_String;

   progversion    : constant String := "2.0.0";
   progname       : constant String := "ravensw";
   install_prefix : constant String := "/raven";
   jail_supported : constant Boolean := False;   --  FreeBSD only

end Core;
