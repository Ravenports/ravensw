--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Directories;
with Interfaces.C.Strings;

package body Core.Repo.Binary_Init is

   package DIR renames Ada.Directories;

   --------------------------------------------------------------------
   --  sqlite_file_exists
   --------------------------------------------------------------------
   procedure sqlite_file_exists
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      db : sqlite_h.sqlite3_Access := sqlite_h.sqlite3_context_db_handle (context);
      db_filename : constant String := SQLite.get_db_filename (db, "main");
      errmsg : IC.Strings.chars_ptr;
      argv   : array (1 .. 2) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if numargs /= 2 then
         errmsg := ICS.New_String ("Invalid usage of file_exists(): needs 2 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      declare
         path := DIR.Containing_Directory (db_filename);
         arg1 : IC.Strings.chars_ptr := sqlite_h.sqlite3_value_text (argv (1))
         fpath : constant String := path & "/" & IC.Strings.Value (arg1);
      begin
         if Unix.valid_permissions (fpath, (flag_read => True, others => False)) then
         else
            sqlite_h.sqlite3_result_int (context, IC.int (0));
         end if;
      end;
   exception
      when DIR.Use_Error =>
         errmsg := ICS.New_String ("file_exists: " & db_filename & " has no containing directory");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      when DIR.Name_Error =>
         errmsg := ICS.New_String ("file_exists: " & db_filename & " can't be identified");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
   end sqlite_file_exists;

end Core.Repo.Binary_Init;
