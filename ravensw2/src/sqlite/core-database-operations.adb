--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Latin_1;
with Interfaces.C.Strings;

with Core.Strings;

with sqlite_h;
with SQLite;

use Core.Strings;

package body Core.Database.Operations is

   package LAT renames Ada.Characters.Latin_1;
   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   --------------------------------------------------------------------
   --  verbatim_command
   --------------------------------------------------------------------
   procedure start_shell (arguments : String)
   is
      numargs : Natural := count_char (arguments, LAT.LF) + 1;
   begin
      if not SQLite.initialize_sqlite then
         return;
      end if;

      declare
         type argv_t is array (1 .. numargs) of aliased ICS.chars_ptr;
         delim : constant String (1 .. 1) := (1 => LAT.LF);
         argv    : argv_t;
         argsval : access ICS.chars_ptr;
         result  : IC.int;
      begin
         for x in 1 .. numargs loop
            argv (x) := ICS.New_String (specific_field (arguments, x, delim));
         end loop;

         argsval := argv (1)'Access;

         result := sqlite_h.sqlite3_shell (IC.int (numargs), argsval);

         for x in 1 .. numargs loop
            ICS.Free (argv (x));
         end loop;
      end;
   end start_shell;


end Core.Database.Operations;
