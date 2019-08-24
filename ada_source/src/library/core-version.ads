--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;  use Core.Strings;

package Core.Version is

   type version_breakdown is
      record
         endname  : Text;
         epoch    : Natural;
         revision : Natural;
      end record;

   --
   --  split_version(pkgname, endname, epoch, revision) returns a pointer to
   --  the version portion of a package name and the two special components.
   --
   --  Syntax is:  ${PORTNAME}-${PORTVERSION}[_${PORTREVISION}][,${PORTEPOCH}]
   --
   --  Written by Oliver Eikemeier
   --  Based on work of Jeremy D. Lea.
   --
   function split_version (pkgname : String) return version_breakdown;

end Core.Version;
