--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Unbounded;

package Core is

   package SU renames Ada.Strings.Unbounded;

   subtype Text is SU.Unbounded_String;
   subtype ST_Debug_Level is Natural range 0 .. 4;
   type T_platform is (generic_unix, freebsd, dragonfly, netbsd, openbsd, linux, macos, solaris);

   progversion    : constant String := "2.0.0";
   progname       : constant String := "ravensw";
   rel_prefix     : constant String := "raven";
   install_prefix : constant String := "/" & rel_prefix;
   jail_supported : constant Boolean := False;   --  FreeBSD only
   platform       : constant T_platform := dragonfly;

   DB_SCHEMA_MAJOR : constant Natural := 0;
   DB_SCHEMA_MINOR : constant Natural := 34;
   DBVERSION       : constant String  := "34"; --  DB_SCHEMA_MAJOR * 1000 + DB_SCHEMA_MINOR

end Core;
