--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;
with sqlite_h;
with System;

package Core.PkgDB is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   procedure pkgshell_open (reponame : access ICS.chars_ptr);
   pragma Export (C, pkgshell_open);

private

   function pkgdb_sqlcmd_init
     (db       : not null sqlite_h.sqlite3_Access;
      pzErrMsg : not null access ICS.chars_ptr;
      pThunk   : not null sqlite_h.sqlite3_api_routines_Access) return IC.int;
   pragma Export (C, pkgdb_sqlcmd_init);

   procedure pkgdb_now
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_now);

   procedure pkgdb_myarch
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_myarch);

   procedure pkgdb_regex
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_regex);

   procedure pkgdb_split_version
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_split_version);

   procedure pkgdb_vercmp
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, pkgdb_vercmp);

end Core.PkgDB;
