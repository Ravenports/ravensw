--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Unix is

   --------------------------------------------------------------------
   --  strerror
   --------------------------------------------------------------------
   function strerror (errno : Integer) return String
   is
      use type IC.Strings.chars_ptr;

      C_Msg : IC.Strings.chars_ptr;
   begin
      C_Msg := C_Strerror (IC.int (errno));

      if C_Msg = IC.Strings.Null_Ptr then
         return "Unknown system error";
      else
         return IC.Strings.Value (C_Msg);
      end if;

   end strerror;


   --------------------------------------------------------------------
   --  errno
   --------------------------------------------------------------------
   function errno return Integer is
   begin
      return last_errno;
   end errno;


   --------------------------------------------------------------------
   --  file_connected
   --------------------------------------------------------------------
   function file_connected (fd : File_Descriptor) return Boolean is
   begin
      return fd /= not_connected;
   end file_connected;


   --------------------------------------------------------------------
   --  success
   --------------------------------------------------------------------
   function success (rc : IC.int) return Boolean
   is
      use type IC.int;
   begin
      return (rc = 0);
   end success;


   --------------------------------------------------------------------
   --  close_file
   --------------------------------------------------------------------
   function close_file (fd : File_Descriptor) return Boolean
   is
      result : IC.int;
   begin
      result := C_Close (IC.int (fd));
      if success (result) then
         return True;
      else
         last_errno := Integer (C_Errno);
         return False;
      end if;
   end close_file;


   --------------------------------------------------------------------
   --  open_file #1
   --------------------------------------------------------------------
   function open_file (filename : String; flags : T_Open_Flags) return File_Descriptor
   is
      result         : File_Descriptor;
      flag_wronly    : IC.int := IC.int (0);
      flag_nonblock  : IC.int := IC.int (0);
      flag_rdonly    : IC.int := IC.int (0);
      flag_directory : IC.int := IC.int (0);
      flag_cloexec   : IC.int := IC.int (0);
      name           : IC.Strings.chars_ptr;
   begin
      if flags.WRONLY then
         flag_wronly := IC.int (1);
      end if;
      if flags.NON_BLOCK then
         flag_nonblock := IC.int (1);
      end if;
      if flags.RDONLY then
         flag_rdonly := IC.int (1);
      end if;
      if flags.DIRECTORY then
         flag_directory := IC.int (1);
      end if;
      if flags.CLOEXEC then
         flag_cloexec := IC.int (1);
      end if;
      name := IC.Strings.New_String (filename);
      result := File_Descriptor (C_Open (path      => name,
                                         rdonly    => flag_rdonly,
                                         wronly    => flag_wronly,
                                         nonblock  => flag_nonblock,
                                         directory => flag_directory,
                                         cloexec   => flag_cloexec));
      IC.Strings.Free (name);
      if not file_connected (result) then
         last_errno := Integer (C_Errno);
      end if;
      return result;
   end open_file;


   --------------------------------------------------------------------
   --  open_file #2
   --------------------------------------------------------------------
   function open_file (dirfd         : File_Descriptor;
                       relative_path : String;
                       flags         : T_Open_Flags) return File_Descriptor
   is
      result         : File_Descriptor;
      flag_wronly    : IC.int := IC.int (0);
      flag_nonblock  : IC.int := IC.int (0);
      flag_rdonly    : IC.int := IC.int (0);
      flag_directory : IC.int := IC.int (0);
      flag_cloexec   : IC.int := IC.int (0);
      name           : IC.Strings.chars_ptr;
   begin
      if flags.WRONLY then
         flag_wronly := IC.int (1);
      end if;
      if flags.NON_BLOCK then
         flag_nonblock := IC.int (1);
      end if;
      if flags.RDONLY then
         flag_rdonly := IC.int (1);
      end if;
      if flags.DIRECTORY then
         flag_directory := IC.int (1);
      end if;
      if flags.CLOEXEC then
         flag_cloexec := IC.int (1);
      end if;
      name := IC.Strings.New_String (relative_path);
      result := File_Descriptor (C_Openat (dirfd     => IC.int (dirfd),
                                           path      => name,
                                           rdonly    => flag_rdonly,
                                           wronly    => flag_wronly,
                                           nonblock  => flag_nonblock,
                                           directory => flag_directory,
                                           cloexec   => flag_cloexec));
      IC.Strings.Free (name);
      if not file_connected (result) then
         last_errno := Integer (C_Errno);
      end if;
      return result;
   end open_file;


   --------------------------------------------------------------------
   --  IPC_mechanism
   --------------------------------------------------------------------
   function IPC_mechanism (filename : String) return Unix_Pipe
   is
      result : IC.int;
      name   : IC.Strings.chars_ptr;
   begin
      name := IC.Strings.New_String (filename);
      result := C_IPC (path => name);
      IC.Strings.Free (name);

      case result is
         when 1 => return named_pipe;
         when 2 => return unix_socket;
         when others => return something_else;
      end case;

   end IPC_mechanism;


   --------------------------------------------------------------------
   --  connect_unix_socket
   --------------------------------------------------------------------
   function connect_unix_socket (filename : String; fd : out File_Descriptor)
                                 return Unix_Socket_Result
   is
      result : IC.int;
      new_fd : IC.int;
      name   : IC.Strings.chars_ptr;
   begin
      name := IC.Strings.New_String (filename);
      result := C_Connect (path  => name, newfd => new_fd);
      IC.Strings.Free (name);

      case result is
         when 1 =>
            fd := File_Descriptor (new_fd);
         when others =>
            fd := not_connected;
            last_errno := Integer (C_Errno);
      end case;

      case result is
         when      1 => return connected;
         when      2 => return failed_creation;
         when      3 => return failed_population;
         when others => return failed_connection;
      end case;
   end connect_unix_socket;


   --------------------------------------------------------------------
   --  push_to_event_pipe
   --------------------------------------------------------------------
   procedure push_to_event_pipe (fd : File_Descriptor; message : String)
   is
      msg    : IC.Strings.chars_ptr;
      result : IC.int;
   begin
      msg := IC.Strings.New_String (message);
      result := C_dprint (IC.int (fd), msg);  -- returns #chars printed
      IC.Strings.Free (msg);
   end push_to_event_pipe;

end Core.Unix;
