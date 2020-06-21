--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.File_Operations is

   file_handling    : exception;

   --  Generic function to scan a text file and convert to a string
   function get_file_contents (dossier : String) return String;

end Core.File_Operations;
