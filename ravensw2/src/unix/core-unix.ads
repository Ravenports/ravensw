--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with System;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package Core.Unix is

   package IC renames Interfaces.C;

   type Unix_Pipe is (named_pipe, unix_socket, something_else);
   type Unix_Socket_Result is (connected, failed_creation, failed_population, failed_connection);

   type Process_ID is new Integer;
   type File_Descriptor is new Integer;
   type uid_t is new Integer;
   not_connected : constant File_Descriptor := -1;

   --  Technically epochtime is a 64-bit signed number, but we've been using time types as
   --  unsigned so far, so keep doing it.
   type T_epochtime is mod 2**64;

   type T_filesize  is mod 2**64;

   --  Set both RDONLY and WRONLY to get RDRW flags
   type T_Open_Flags is
      record
         RDONLY    : Boolean := False;
         WRONLY    : Boolean := False;
         NON_BLOCK : Boolean := False;
         DIRECTORY : Boolean := False;
         CLOEXEC   : Boolean := False;
         CREAT     : Boolean := False;
         TRUNC     : Boolean := False;
      end record;

   type T_Access_Flags is
      record
         flag_read  : Boolean;
         flag_write : Boolean;
         flag_exec  : Boolean;
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

   --  Return true if teststring matches the pattern according to the shell rules.
   function filename_match (pattern, teststring : String) return Boolean;

   --  Set errno to zero
   procedure reset_errno;
   pragma Import (C, reset_errno, "reset_errno");

   --  Get Process ID
   function getpid return Process_ID;
   pragma Import (C, getpid, "getpid");

   function C_Openat_Stock
     (dirfd     : IC.int;
      path      : IC.Strings.chars_ptr;
      flags     : IC.int;
      mode      : IC.int) return IC.int;
   pragma Import (C, C_Openat_Stock, "openat");

   function C_faccessat
     (dfd  : IC.int;
      path : IC.Strings.chars_ptr;
      mode : IC.int;
      flag : IC.int) return IC.int;
   pragma Import (C, C_faccessat, "port_faccessat");

   function C_unlinkat
     (dfd  : unix.File_Descriptor;
      path : IC.Strings.chars_ptr;
      remove_dir : IC.int) return IC.int;
   pragma Import (C, C_unlinkat, "port_unlinkat");

   type struct_stat is limited private;
   type struct_stat_Access is access all struct_stat;
   pragma Convention (C, struct_stat_Access);

   function C_fstatat
     (dfd  : IC.int;
      path : IC.Strings.chars_ptr;
      sb   : struct_stat_Access;
      flag : IC.int) return IC.int;
   pragma Import (C, C_fstatat, "fstatat");

   function lstatat
     (dfd  : File_Descriptor;
      path : String;
      sb   : struct_stat_Access) return Boolean;

   function C_mkdirat
     (dfd  : IC.int;
      path : IC.Strings.chars_ptr;
      mode : IC.int) return IC.int;
   pragma Import (C, C_mkdirat, "port_mkdirat");

   function relative_file_readable
     (dfd  : File_Descriptor;
      path : String) return Boolean;

   function relative_file_writable
     (dfd  : File_Descriptor;
      path : String) return Boolean;

   function get_current_working_directory return String;

   function getuid return uid_t;
   pragma Import (C, getuid, "getuid");

   function geteuid return uid_t;
   pragma Import (C, geteuid, "geteuid");

   function getgid return uid_t;
   pragma Import (C, getgid, "getgid");

   function getegid return uid_t;
   pragma Import (C, getegid, "getegid");

   function stat_ok (path : String; sb : struct_stat_Access) return Boolean;

   function lstat_ok (path : String; sb : struct_stat_Access) return Boolean;

   function last_error_ACCESS return Boolean;

   function last_error_NOENT return Boolean;

   function bad_perms (fileowner : uid_t; filegroup : uid_t; sb : struct_stat_Access)
                       return Boolean;
   function wrong_owner (fileowner : uid_t; filegroup : uid_t; sb : struct_stat_Access)
                         return Boolean;

   function valid_permissions (path : String; permissions : T_Access_Flags) return Boolean;

   function get_mtime (sb : struct_stat_Access) return T_epochtime;

   function get_file_size (sb : struct_stat_Access) return T_filesize;

   procedure set_file_times (path : String;
                             access_time : T_epochtime;
                             mod_time : T_epochtime);

   function read_fd (fd : File_Descriptor; max_bytes : Natural) return String;

   function readlink (path : String) return String;
   function readlink (fd : File_Descriptor; relative_path : String) return String;

   function is_link (sb : struct_stat_Access) return Boolean;

   --   Attempts to set mode.  Returns -1 on failure, and 0 .. 0xFFFF on success
   function get_mode (mode_str : String) return Integer;

   --  Returns True if kill operation was successful
   function kill (pid : Process_ID) return Boolean;

   --  Returns True if a TTY device is detected
   function screen_attached return Boolean;

   --  Returns True in unlink operation success
   function unlink (path : String) return Boolean;
   function unlink (fd            : File_Descriptor;
                    relative_path : String;
                    is_directory  : Boolean := False) return Boolean;

   --  Returns True if lseek operation successful
   function reset_file_for_reading (fd : Unix.File_Descriptor) return Boolean;

private

   last_errno : Integer;

   type stat_block is array (1 .. 256) of IC.unsigned_char;
   type struct_stat is limited
      record
         --  sizeof(struct stat) is 128 on DragonFly
         --  Double that to ensure we allocate enough
         block : stat_block;
      end record;

   function success (rc : IC.int) return Boolean;

   function C_Strerror (Errnum : IC.int) return IC.Strings.chars_ptr;
   pragma Import (C, C_Strerror, "strerror");

   function C_Close (fd : IC.int) return IC.int;
   pragma Import (C, C_Close, "close");

   function C_Errno return IC.int;
   pragma Import (C, C_Errno, "get_errno");

   function C_Open
     (path      : IC.Strings.chars_ptr;
      rdonly    : IC.int;
      wronly    : IC.int;
      nonblock  : IC.int;
      directory : IC.int;
      cloexec   : IC.int;
      creat     : IC.int;
      trunc     : IC.int) return IC.int;
   pragma Import (C, C_Open, "try_open");

   function C_Openat
     (dirfd     : IC.int;
      path      : IC.Strings.chars_ptr;
      rdonly    : IC.int;
      wronly    : IC.int;
      nonblock  : IC.int;
      directory : IC.int;
      cloexec   : IC.int;
      creat     : IC.int;
      trunc     : IC.int) return IC.int;
   pragma Import (C, C_Openat, "try_openat");

   function C_IPC (path : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_IPC, "detect_IPC");

   function C_Connect (path  : IC.Strings.chars_ptr;
                       newfd : out IC.int) return IC.int;
   pragma Import (C, C_Connect, "connect_socket");

   function C_dprint (fd : IC.int; msg : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_dprint, "dprint");

   function C_fnmatch (pattern, teststring : IC.Strings.chars_ptr; flags : IC.int) return IC.int;
   pragma Import (C, C_fnmatch, "fnmatch");

   function C_lstatat
     (dfd  : IC.int;
      path : IC.Strings.chars_ptr;
      sb   : struct_stat_Access) return IC.int;
   pragma Import (C, C_lstatat, "port_lstatat");

   function C_stat
     (path : IC.Strings.chars_ptr;
      sb   : struct_stat_Access) return IC.int;
   pragma Import (C, C_stat, "stat");

   function C_lstat
     (path : IC.Strings.chars_ptr;
      sb   : struct_stat_Access) return IC.int;
   pragma Import (C, C_lstat, "lstat");

   function C_faccessat_readable
     (dfd  : IC.int;
      path : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_faccessat_readable, "port_faccessat_readable");

   function C_faccessat_writable
     (dfd  : IC.int;
      path : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_faccessat_writable, "port_faccessat_writable");

   function C_getcwd
     (buf  : IC.Strings.chars_ptr;
      size : IC.size_t) return IC.Strings.chars_ptr;
   pragma Import (C, C_getcwd, "getcwd");

   function C_errno_EACCESS return IC.int;
   pragma Import (C, C_errno_EACCESS, "last_error_ACCES");

   function C_errno_ENOENT return IC.int;
   pragma Import (C, C_errno_ENOENT, "last_error_NOENT");

   function C_bad_perms  (fileowner, filegroup : IC.int; sb : struct_stat_Access)
      return IC.int;
   pragma Import (C, C_bad_perms, "bad_perms");

   function C_wrong_owner  (fileowner, filegroup : IC.int; sb : struct_stat_Access)
      return IC.int;
   pragma Import (C, C_wrong_owner, "wrong_owner");

   function C_access (path : IC.Strings.chars_ptr; mode : IC.int) return IC.int;
   pragma Import (C, C_access, "access");

   function C_get_mtime (sb : struct_stat_Access) return IC.long;
   pragma Import (C, C_get_mtime, "get_mtime");

   function C_get_size (sb : struct_stat_Access) return IC.Extensions.long_long;
   pragma Import (C, C_get_size, "get_size");

   type Timeval_Unit is new IC.int;
   pragma Convention (C, Timeval_Unit);

   type Timeval is record
      Tv_Sec  : Timeval_Unit;
      Tv_Usec : Timeval_Unit;
   end record;
   pragma Convention (C, Timeval);

   type Timeval_Access is access all Timeval;
   pragma Convention (C, Timeval_Access);

   function C_utimes (path : IC.Strings.chars_ptr; times : Timeval_Access) return IC.int;
   pragma Import (C, C_utimes, "utimes");

   function C_read (fd : File_Descriptor; buf : access IC.unsigned_char; nbytes : IC.size_t)
                    return IC.Extensions.long_long;
   pragma Import (C, C_read, "read");

   function C_readlink (path   : IC.Strings.chars_ptr;
                        buf    : access IC.unsigned_char;
                        bufsiz : IC.size_t) return IC.Extensions.long_long;
   pragma Import (C, C_readlink, "readlink");

   function C_readlinkat (fd     : File_Descriptor;
                          path   : IC.Strings.chars_ptr;
                          buf    : access IC.unsigned_char;
                          bufsiz : IC.size_t) return IC.Extensions.long_long;
   pragma Import (C, C_readlinkat, "port_readlinkat");

   function C_is_link  (sb : struct_stat_Access) return IC.int;
   pragma Import (C, C_is_link, "is_link");

   function C_setmode (mode_str : IC.Strings.chars_ptr) return IC.Extensions.void_ptr;
   pragma Import (C, C_setmode, "port_setmode");

   function C_getmode (set  : IC.Extensions.void_ptr;
                       mode : IC.unsigned_short) return IC.unsigned_short;
   pragma Import (C, C_getmode, "port_getmode");

   procedure C_free (Ptr : System.Address);
   pragma Import (C, C_free, "free");

   function C_kill (pid : Process_ID; sig : IC.int) return IC.int;
   pragma Import (C, C_kill, "kill");

   function C_unlink (path : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_unlink, "unlink");

   function C_lseek (fildes : File_Descriptor;
                     offset : IC.Extensions.long_long;
                     whence : IC.int) return IC.Extensions.long_long;
   pragma Import (C, C_lseek, "lseek");

end Core.Unix;
