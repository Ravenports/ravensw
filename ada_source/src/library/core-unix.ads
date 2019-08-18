--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;

package Core.Unix is

   package IC renames Interfaces.C;

   type Unix_Pipe is (named_pipe, unix_socket, something_else);
   type Unix_Socket_Result is (connected, failed_creation, failed_population, failed_connection);

   type File_Descriptor is new Integer;
   not_connected : constant File_Descriptor := 1;

   type T_Open_Flags is
      record
         RDONLY    : Boolean := False;
         WRONLY    : Boolean := False;
         NON_BLOCK : Boolean := False;
         DIRECTORY : Boolean := False;
         CLOEXEC   : Boolean := False;

      end record;

   --  strerror from libc
   function strerror (errno : Integer) return String;

   --  call C function to determine if FIFO or Socket or something else
   function IPC_mechanism (filename : String) return Unix_Pipe;

   --  Use libc's open function to retrieve file descriptor
   function open_file (filename : String; flags : T_Open_Flags) return File_Descriptor;

   --  Use libc's openat function to retrieve file descriptor
   function open_file (dirfd         : File_Descriptor;
                       relative_path : String;
                       flags         : T_Open_Flags) return File_Descriptor;

   --  Last seen error number by C function
   function errno return Integer;

   --  Connect to Unix-style socket
   function connect_unix_socket (filename : String; fd : out File_Descriptor)
                                 return Unix_Socket_Result;

   --  Return True if fd /= -1
   function file_connected (fd : File_Descriptor) return Boolean;

   --  Return True if file was successfully closed
   function close_file (fd : File_Descriptor) return Boolean;

   --  Send log down file descriptor of event pipe
   procedure push_to_event_pipe (fd : File_Descriptor; message : String);

   --  Set errno to zero
   procedure reset_errno;
   pragma Import (C, reset_errno, "reset_errno");

private

   last_errno : Integer;

   function success (rc : IC.int) return Boolean;

   function C_Strerror (Errnum : IC.int) return IC.Strings.chars_ptr;
   pragma Import (C, C_Strerror, "strerror");

   function C_Close (fd : IC.int) return IC.int;
   pragma Import (C, C_Close, "close");

   function C_Errno return IC.int;
   pragma Import (C, C_Errno, "get_errno");

   function C_Open (path      : IC.Strings.chars_ptr;
                    rdonly    : IC.int;
                    wronly    : IC.int;
                    nonblock  : IC.int;
                    directory : IC.int;
                    cloexec   : IC.int) return IC.int;
   pragma Import (C, C_Open, "try_open");

   function C_Openat (dirfd     : IC.int;
                      path      : IC.Strings.chars_ptr;
                      rdonly    : IC.int;
                      wronly    : IC.int;
                      nonblock  : IC.int;
                      directory : IC.int;
                      cloexec   : IC.int) return IC.int;
   pragma Import (C, C_Openat, "try_openat");

   function C_IPC (path : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_IPC, "detect_IPC");

   function C_Connect (path  : IC.Strings.chars_ptr;
                       newfd : out IC.int) return IC.int;
   pragma Import (C, C_Connect, "connect_socket");

   function C_dprint (fd : IC.int; msg : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_dprint, "dprint");


end Core.Unix;
