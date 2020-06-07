--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

pragma Style_Checks (Off);

with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with System;

package fetch_h is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   URL_SCHEMELEN  : constant := 16;
   URL_USERLEN    : constant := 256;
   URL_PWDLEN     : constant := 256;
   MAXHOSTNAMELEN : constant := 256;
   PATH_MAX       : constant := 1024;
   IPPORT_MAX     : constant := 65535;

   SCHEME_FTP   : aliased constant String := "ftp" & ASCII.NUL;
   SCHEME_HTTP  : aliased constant String := "http" & ASCII.NUL;
   SCHEME_HTTPS : aliased constant String := "https" & ASCII.NUL;
   SCHEME_FILE  : aliased constant String := "file" & ASCII.NUL;

   FETCH_ABORT   : constant := 1;
   FETCH_AUTH    : constant := 2;
   FETCH_DOWN    : constant := 3;
   FETCH_EXISTS  : constant := 4;
   FETCH_FULL    : constant := 5;
   FETCH_INFO    : constant := 6;
   FETCH_MEMORY  : constant := 7;
   FETCH_MOVED   : constant := 8;
   FETCH_NETWORK : constant := 9;
   FETCH_OK      : constant := 10;
   FETCH_PROTO   : constant := 11;
   FETCH_RESOLV  : constant := 12;
   FETCH_SERVER  : constant := 13;
   FETCH_TEMP    : constant := 14;
   FETCH_TIMEOUT : constant := 15;
   FETCH_UNAVAIL : constant := 16;
   FETCH_UNKNOWN : constant := 17;
   FETCH_URL     : constant := 18;
   FETCH_VERBOSE : constant := 19;

   MAXERRSTRING  : constant := 256;

   subtype time_t is Interfaces.Integer_64;
   subtype anon884_scheme_array is Interfaces.C.char_array (0 .. 16);
   subtype anon884_user_array   is Interfaces.C.char_array (0 .. 256);
   subtype anon884_pwd_array    is Interfaces.C.char_array (0 .. 256);
   subtype anon884_host_array   is Interfaces.C.char_array (0 .. 256);
   type url is record
      scheme   : aliased anon884_scheme_array;
      user     : aliased anon884_user_array;
      pwd      : aliased anon884_pwd_array;
      host     : aliased anon884_host_array;
      port     : aliased IC.int;
      doc      : ICS.chars_ptr;
      offset   : aliased IC.int;
      length   : aliased IC.size_t;
      ims_time : aliased time_t;
      netrcfd  : aliased IC.int;
   end record;
   pragma Convention (C_Pass_By_Copy, url);

   type url_stat is record
      size  : aliased IC.int;
      atime : aliased time_t;
      mtime : aliased time_t;
   end record;
   pragma Convention (C_Pass_By_Copy, url_stat);

   subtype anon890_name_array is Interfaces.C.char_array (0 .. 1023);
   type url_ent is record
      name : aliased anon890_name_array;
      stat : aliased url_stat;
   end record;
   pragma Convention (C_Pass_By_Copy, url_ent);

   type estream_t is limited private;
   type Extended_Stream is access all estream_t;
   pragma Convention (C, Extended_Stream);

   ------------------------------------------------------------------
   --  FILE-specific functions
   ------------------------------------------------------------------
   function fetchXGetFile
     (arg1 : access url;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchXGetFile, "fetchXGetFile");

   function fetchGetFile
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchGetFile, "fetchGetFile");

   function fetchPutFile
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchPutFile, "fetchPutFile");

   function fetchStatFile
     (arg1 : access url;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return IC.int;
   pragma Import (C, fetchStatFile, "fetchStatFile");

   function fetchListFile
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return access url_ent;
   pragma Import (C, fetchListFile, "fetchListFile");

   ------------------------------------------------------------------
   --  HTTP-specific functions
   ------------------------------------------------------------------
   function fetchXGetHTTP
     (arg1 : access url;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchXGetHTTP, "fetchXGetHTTP");

   function fetchGetHTTP
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchGetHTTP, "fetchGetHTTP");

   function fetchPutHTTP
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchPutHTTP, "fetchPutHTTP");

   function fetchStatHTTP
     (arg1 : access url;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return IC.int;
   pragma Import (C, fetchStatHTTP, "fetchStatHTTP");

   function fetchListHTTP
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return access url_ent;
   pragma Import (C, fetchListHTTP, "fetchListHTTP");

   function fetchReqHTTP
     (arg1 : access url;
      arg2 : ICS.chars_ptr;
      arg3 : ICS.chars_ptr;
      arg4 : ICS.chars_ptr;
      arg5 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchReqHTTP, "fetchReqHTTP");

   ------------------------------------------------------------------
   --  FTP-specific functions
   ------------------------------------------------------------------
   function fetchXGetFTP
     (arg1 : access url;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchXGetFTP, "fetchXGetFTP");

   function fetchGetFTP
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchGetFTP, "fetchGetFTP");

   function fetchPutFTP
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchPutFTP, "fetchPutFTP");

   function fetchStatFTP
     (arg1 : access url;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return IC.int;
   pragma Import (C, fetchStatFTP, "fetchStatFTP");

   function fetchListFTP
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return access url_ent;
   pragma Import (C, fetchListFTP, "fetchListFTP");

   ------------------------------------------------------------------
   --  Generic functions
   ------------------------------------------------------------------
   function fetchXGetURL
     (arg1 : ICS.chars_ptr;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchXGetURL, "fetchXGetURL");

   function fetchGetURL
     (arg1 : ICS.chars_ptr;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchGetURL, "fetchGetURL");

   function fetchPutURL
     (arg1 : ICS.chars_ptr;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchPutURL, "fetchPutURL");

   function fetchStatURL
     (arg1 : ICS.chars_ptr;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return IC.int;
   pragma Import (C, fetchStatURL, "fetchStatURL");

   function fetchListURL
     (arg1 : ICS.chars_ptr;
      arg2 : ICS.chars_ptr) return access url_ent;
   pragma Import (C, fetchListURL, "fetchListURL");

   function fetchXGet
     (arg1 : access url;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchXGet, "fetchXGet");

   function fetchGet
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchGet, "fetchGet");

   function fetchPut
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return Extended_Stream;
   pragma Import (C, fetchPut, "fetchPut");

   function fetchStat
     (arg1 : access url;
      arg2 : access url_stat;
      arg3 : ICS.chars_ptr) return IC.int;
   pragma Import (C, fetchStat, "fetchStat");

   function fetchList
     (arg1 : access url;
      arg2 : ICS.chars_ptr) return access url_ent;
   pragma Import (C, fetchList, "fetchList");

   ------------------------------------------------------------------
   --  URL parsing
   ------------------------------------------------------------------
   function fetchMakeURL
     (arg1 : ICS.chars_ptr;
      arg2 : ICS.chars_ptr;
      arg3 : IC.int;
      arg4 : ICS.chars_ptr;
      arg5 : ICS.chars_ptr;
      arg6 : ICS.chars_ptr) return access url;
   pragma Import (C, fetchMakeURL, "fetchMakeURL");

   function fetchParseURL (arg1 : ICS.chars_ptr) return access url;
   pragma Import (C, fetchParseURL, "fetchParseURL");

   procedure fetchFreeURL (arg1 : access url);
   pragma Import (C, fetchFreeURL, "fetchFreeURL");

   ------------------------------------------------------------------
   --  Authentication
   ------------------------------------------------------------------
   type auth_t is access function (arg1 : access url) return IC.int;
   pragma Convention (C, auth_t);

   fetchAuthMethod : auth_t;
   pragma Import (C, fetchAuthMethod, "fetchAuthMethod");

   ------------------------------------------------------------------
   --  Last error code
   ------------------------------------------------------------------
   fetchLastErrCode : aliased IC.int;
   pragma Import (C, fetchLastErrCode, "fetchLastErrCode");

   fetchLastErrString : aliased Interfaces.C.char_array (0 .. 255);
   pragma Import (C, fetchLastErrString, "fetchLastErrString");

   ------------------------------------------------------------------
   --  I/O timeout
   ------------------------------------------------------------------
   fetchTimeout : aliased IC.int;
   pragma Import (C, fetchTimeout, "fetchTimeout");

   ------------------------------------------------------------------
   --  Restart interrupted syscalls
   ------------------------------------------------------------------
   fetchRestartCalls : aliased IC.int;
   pragma Import (C, fetchRestartCalls, "fetchRestartCalls");

   ------------------------------------------------------------------
   --  Extra verbosity
   ------------------------------------------------------------------
   fetchDebug : aliased IC.int;
   pragma Import (C, fetchDebug, "fetchDebug");

   ------------------------------------------------------------------
   --  Extended Stream functions used by ravensw
   ------------------------------------------------------------------
   function es_fclose (arg1 : Extended_Stream) return IC.int;
   pragma Import (C, es_fclose, "es_fclose");

   function es_ferror (arg1 : Extended_Stream) return IC.int;
   pragma Import (C, es_ferror, "es_ferror");

   function es_fread
     (arg1 : access IC.char;
      arg2 : IC.size_t;
      arg3 : IC.size_t;
      arg4 : Extended_Stream) return IC.size_t;
   pragma Import (C, es_fread, "es_fread");

   function es_getline
     (arg1 : system.Address;
      arg2 : access IC.size_t;
      arg3 : Extended_Stream) return IC.Extensions.long_long;
   pragma Import (C, es_getline, "es_getline");

   function es_fprintf
     (arg1 : Extended_Stream;
      arg2 : ICS.chars_ptr) return IC.int;
   pragma Import (C, es_fprintf, "es_fprintf");

   type es_cookie_read_function_t is access function
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : IC.size_t) return IC.Extensions.long_long;
   pragma Convention (C, es_cookie_read_function_t);

   type es_cookie_write_function_t is access function
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : IC.size_t) return IC.Extensions.long_long;
   pragma Convention (C, es_cookie_write_function_t);

   type es_cookie_seek_function_t is access function
     (arg1 : System.Address;
      arg2 : access IC.long;
      arg3 : IC.int) return IC.int;
   pragma Convention (C, es_cookie_seek_function_t);

   type es_cookie_close_function_t is access function
     (arg1 : System.Address) return IC.int;
   pragma Convention (C, es_cookie_close_function_t);

   type es_cookie_io_functions_t is record
      func_read  : es_cookie_read_function_t;
      func_write : es_cookie_write_function_t;
      func_seek  : es_cookie_seek_function_t;
      func_close : es_cookie_close_function_t;
   end record;
   pragma Convention (C_Pass_By_Copy, es_cookie_io_functions_t);

   function es_fopencookie
     (cookie    : System.Address;
      mode      : ICS.chars_ptr;
      functions : es_cookie_io_functions_t) return Extended_Stream;
   pragma Import (C, es_fopencookie, "es_fopencookie");

private

   type estream_t is limited null record;

end fetch_h;
