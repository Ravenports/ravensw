--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Unbounded;

package Core is

   package SU renames Ada.Strings.Unbounded;

   type Operating_System is (generic_unix,
                             freebsd,
                             dragonfly,
                             netbsd,
                             openbsd,
                             linux,
                             macos,
                             solaris,
                             omnios);

   progversion    : constant String := "2.0.0";
   progname       : constant String := "ravensw";
   rel_prefix     : constant String := "raven";
   install_prefix : constant String := "/" & rel_prefix;
   platform       : constant Operating_System := dragonfly;
   jail_supported : constant Boolean := (platform = freebsd);

   DB_SCHEMA_MAJOR : constant Natural := 0;
   DB_SCHEMA_MINOR : constant Natural := 35;
   DB_SCHEMA_ALL   : constant Natural := DB_SCHEMA_MAJOR * 1000 + DB_SCHEMA_MINOR;
   DBVERSION       : constant String  := "35"; --  DB_SCHEMA_ALL

   --  Global Types
   subtype Text is SU.Unbounded_String;
   subtype ST_Debug_Level is Natural range 0 .. 4;
   type int64 is range -(2**63) .. +(2**63 - 1);
   type Action_Result is
     (RESULT_OK,         --  Nominal
      RESULT_END,        --  No more items available (end of the loop)
      RESULT_WARN,       --  The function encountered a non-fatal error
      RESULT_FATAL,      --  The function encountered a fatal error
      RESULT_REQUIRED,   --  Can not delete the package because it is required by another package
      RESULT_INSTALLED,  --  Can not install the package because it is already installed.
      RESULT_DEPENDENCY, --  Can not install the package because some dependencies are unresolved
      RESULT_LOCKED,     --  Can not operate on package because it is locked
      RESULT_ENODB,      --  Can not create local database or database non-existent
      RESULT_UPTODATE,   --  local file newer than remote
      RESULT_UNKNOWN,    --  unknown keyword
      RESULT_REPOSCHEMA, --  repo DB schema incompatible version
      RESULT_ENOACCESS,  --  Insufficient privilege for action
      RESULT_INSECURE,   --  Insecure permissions on any component of local.sqlite database
      RESULT_CONFLICT,   --  A conflict between packages found
      RESULT_AGAIN,      --  Need to repeat operation
      RESULT_NOTINST,    --  Not installed
      RESULT_VITAL       --  Can not delete the package because it is vital, i.e. a kernel
     );
   type Init_protocol is (INIT_NONE, INIT_USE_IPV4, INIT_USE_IPV6);

end Core;
