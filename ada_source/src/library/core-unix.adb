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
      if fd = not_connected then
         return False;
      end if;

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


   --------------------------------------------------------------------
   --  filename_match
   --------------------------------------------------------------------
   function filename_match (pattern, teststring : String) return Boolean
   is
      c_pattern    : IC.Strings.chars_ptr;
      c_teststring : IC.Strings.chars_ptr;
      result       : IC.int;
   begin
      c_pattern := IC.Strings.New_String (pattern);
      c_teststring := IC.Strings.New_String (teststring);
      result := C_fnmatch (c_pattern, c_teststring, 0);
      IC.Strings.Free (c_pattern);
      IC.Strings.Free (c_teststring);
      return success (result);
   end filename_match;


   --------------------------------------------------------------------
   --  lstatat
   --------------------------------------------------------------------
   function lstatat
     (dfd  : File_Descriptor;
      path : String;
      sb   : struct_stat_Access) return Boolean
   is
      c_path : IC.Strings.chars_ptr;
      result       : IC.int;
   begin
      c_path := IC.Strings.New_String (path);
      result := C_lstat (dfd  => IC.int (dfd),
                         path => c_path,
                         sb   => sb);
      IC.Strings.Free (c_path);
      return success (result);
   end lstatat;


   --------------------------------------------------------------------
   --  relative_file_readable
   --------------------------------------------------------------------
   function relative_file_readable
     (dfd  : File_Descriptor;
      path : String) return Boolean
   is
      c_path : IC.Strings.chars_ptr;
      result : IC.int;
   begin
      c_path := IC.Strings.New_String (path);
      result := C_faccessat_readable (IC.int (dfd), c_path);
      IC.Strings.Free (c_path);
      return success (result);
   end relative_file_readable;


   --------------------------------------------------------------------
   --  relative_file_writable
   --------------------------------------------------------------------
   function relative_file_writable
     (dfd  : File_Descriptor;
      path : String) return Boolean
   is
      c_path : IC.Strings.chars_ptr;
      result : IC.int;
   begin
      c_path := IC.Strings.New_String (path);
      result := C_faccessat_writable (IC.int (dfd), c_path);
      IC.Strings.Free (c_path);
      return success (result);
   end relative_file_writable;


   --------------------------------------------------------------------
   --  get_current_working_directory
   --------------------------------------------------------------------
   function get_current_working_directory return String
   is
      use type IC.Strings.chars_ptr;

      ptr_to_dest : IC.Strings.chars_ptr;
   begin
      ptr_to_dest := C_getcwd (IC.Strings.Null_Ptr, IC.size_t (0));  -- will resize to MAXPATHLEN
      if ptr_to_dest = IC.Strings.Null_Ptr then
         return "";
      end if;

      declare
         result : String := IC.Strings.Value (ptr_to_dest);
      begin
         IC.Strings.Free (ptr_to_dest);
         return result;
      end;
   end get_current_working_directory;


   --------------------------------------------------------------------
   --  stat_ok
   --------------------------------------------------------------------
   function stat_ok (path : String; sb : struct_stat_Access) return Boolean
   is
      use type IC.int;
      c_path : IC.Strings.chars_ptr;
      res : IC.int;
   begin
      c_path := IC.Strings.New_String (path);
      res := C_stat (c_path, sb);
      IC.Strings.Free (c_path);
      return (res = IC.int (0));
   end stat_ok;


   --------------------------------------------------------------------
   --  last_error_ACCESS
   --------------------------------------------------------------------
   function last_error_ACCESS return Boolean
   is
      use type IC.int;
      target_error : IC.int := C_errno_EACCESS;
   begin
      return (target_error /= IC.int (0));
   end last_error_ACCESS;


   --------------------------------------------------------------------
   --  last_error_NOENT
   --------------------------------------------------------------------
   function last_error_NOENT return Boolean
   is
      use type IC.int;
      target_error : IC.int := C_errno_ENOENT;
   begin
      return (target_error /= IC.int (0));
   end last_error_NOENT;


   --------------------------------------------------------------------
   --  bad_perms
   --------------------------------------------------------------------
   function bad_perms (fileowner : uid_t; filegroup : uid_t; sb : struct_stat) return Boolean
   is
      use type IC.int;
      res : IC.int := C_bad_perms (IC.int (fileowner), IC.int (filegroup), sb);
   begin
      return (res = IC.int (1));
   end bad_perms;


   --------------------------------------------------------------------
   --  wrong_owner
   --------------------------------------------------------------------
   function wrong_owner (fileowner : uid_t; filegroup : uid_t; sb : struct_stat) return Boolean
   is
      use type IC.int;
      res : IC.int := C_wrong_owner (IC.int (fileowner), IC.int (filegroup), sb);
   begin
      return (res = IC.int (1));
   end wrong_owner;


   --------------------------------------------------------------------
   --  wrong_owner
   --------------------------------------------------------------------
   function valid_permissions (path : String; permissions : T_Access_Flags) return Boolean
   is
      use type IC.int;

      mode   : IC.int := IC.int (0);
      res    : IC.int;
      c_path : IC.Strings.chars_ptr;
   begin
      if permissions.flag_read then
         mode := mode + IC.int (2 ** 0);
      end if;
      if permissions.flag_write then
         mode := mode + IC.int (2 ** 1);
      end if;
      if permissions.flag_exec then
         mode := mode + IC.int (2 ** 2);
      end if;

      c_path := IC.Strings.New_String (path);
      res := C_access (c_path, mode);
      IC.Strings.Free (c_path);
      return (res = IC.int (0));
   end valid_permissions;

end Core.Unix;
