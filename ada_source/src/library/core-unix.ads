--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;

package Core.Unix is

   package IC renames Interfaces.C;

   type Unix_Pipe is (named_pipe, unix_socket, something_else);
   type Unix_Socket_Result is (connected, failed_creation, failed_population, failed_connection);

   type Unix_File_Descriptor is new Integer;
   not_connected : constant Unix_File_Descriptor := 1;

   --  strerror from libc
   function strerror (errno : Integer) return String;

   --  call C function to determine if FIFO or Socket or something else
   function IPC_mechanism (filename : String) return Unix_Pipe;

   --  Use libc's open function to retrieve file descriptor
   function open (filename : String; WRONLY, NON_BLOCK : Boolean) return Unix_File_Descriptor;

   function close (fd : Unix_File_Descriptor) return Boolean;

   --  Last seen error number by C function
   function errno return Integer;

   --  Connect to Unix-style socket
   function connect_unix_socket (filename : String; fd : out Unix_File_Descriptor)
                                 return Unix_Socket_Result;

private

   last_errno : Integer;

   function C_Strerror (Errnum : IC.int) return IC.Strings.chars_ptr;
   pragma Import (C, C_Strerror, "strerror");

end Core.Unix;
