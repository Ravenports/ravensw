--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Directories;
with Interfaces.C.Strings;

with Core.Unix;
with Core.Checksum;
with SQLite;

package body Core.Repo.Binary_Init is

   package DIR renames Ada.Directories;
   package ICS renames Interfaces.C.Strings;

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

      use type IC.int;
   begin
      if numargs /= 2 then
         errmsg := IC.Strings.New_String ("Invalid usage of file_exists(): needs 2 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      declare
         path : String := DIR.Containing_Directory (db_filename);
         arg1 : IC.Strings.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         fpath : constant String := path & "/" & ICS.Value (arg1);
      begin
         if Unix.valid_permissions (fpath, (flag_read => True, others => False)) then
            declare
               cksum : String := Checksum.pkg_checksum_file (fpath, PKG_HASH_TYPE_SHA256_HEX);
            begin
               if cksum = ICS.Value (arg1) then
                  sqlite_h.sqlite3_result_int (context, IC.int (1));
               else
                  sqlite_h.sqlite3_result_int (context, IC.int (0));
               end if;
            end;
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


   --------------------------------------------------------------------
   --  pkg_repo_binary_init
   --------------------------------------------------------------------
   function pkg_repo_binary_init (reponame : Text) return Boolean is
   begin
      --  TODO:
      return False;
   end pkg_repo_binary_init;

end Core.Repo.Binary_Init;
