--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;

package body Core.File_Operations is

   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------
   --  get_file_contents
   --------------------------------------------------------------------
   function get_file_contents (dossier : String) return String
   is
      File_Size : constant Natural := Natural (DIR.Size (dossier));
   begin
      if File_Size = 0 then
         return "";
      end if;

      declare
         subtype File_String    is String (1 .. File_Size);
         package File_String_IO is new Ada.Direct_IO (File_String);

         file_handle : File_String_IO.File_Type;
         contents    : File_String;
      begin

         begin
            File_String_IO.Open  (File => file_handle,
                                  Mode => File_String_IO.In_File,
                                  Name => dossier);
         exception
            when File_String_IO.Status_Error
               | File_String_IO.Use_Error =>
               raise file_handling with "get_file_contents: failed open: " & dossier;
         end;
         File_String_IO.Read  (File => file_handle, Item => contents);
         File_String_IO.Close (file_handle);
         return contents;
      exception
         when others =>
            if File_String_IO.Is_Open (file_handle) then
               File_String_IO.Close (file_handle);
            end if;
            raise file_handling with "get_file_contents(" & dossier & ") failed";
      end;
   exception
      when Storage_Error =>
         raise file_handling with "get_file_contents(" & dossier & ") failed to allocate memory";
   end get_file_contents;

end Core.File_Operations;
