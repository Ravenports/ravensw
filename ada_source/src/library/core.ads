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

   type Pkg_Error_Type is
     (
      EPKG_OK,
      EPKG_END,        --  No more items available (end of the loop)
      EPKG_WARN,       --  The function encountered a non-fatal error
      EPKG_FATAL,      --  The function encountered a fatal error
      EPKG_REQUIRED,   --  Can not delete the package because it is required by another package
      EPKG_INSTALLED,  --  Can not install the package because it is already installed.
      EPKG_DEPENDENCY, --  Can not install the package because some dependencies are unresolved
      EPKG_LOCKED,     --  Can not operate on package because it is locked
      EPKG_ENODB,      --  Can not create local database or database non-existent
      EPKG_UPTODATE,   --  local file newer than remote
      EPKG_UNKNOWN,    --  unkown keyword
      EPKG_REPOSCHEMA, --  repo DB schema incompatible version
      EPKG_ENOACCESS,  --  Insufficient privilege for action
      EPKG_INSECURE,   --  Insecure permissions on any component of $PKGDB_DIR/local.sqlite or
                       --  any of the repo database bits
      EPKG_CONFLICT,   --  A conflict between packages found
      EPKG_AGAIN,      --  Need to repeat operation
      EPKG_NOTINST,    --  Not installed
      EPKG_VITAL       -- Can not delete the package because it is vital, i.e. a kernel
     );


end Core;
