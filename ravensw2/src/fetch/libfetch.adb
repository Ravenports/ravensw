--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Interfaces.C.Extensions;

package body Libfetch is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   --------------------------------------------------------------------
   --  last_fetch_ok
   --------------------------------------------------------------------
   function last_fetch_ok return Boolean
   is
      use type IC.int;
   begin
      return (fetch_h.fetchLastErrCode = fetch_h.FETCH_OK);
   end last_fetch_ok;


   --------------------------------------------------------------------
   --  last_fetch_unavailable
   --------------------------------------------------------------------
   function last_fetch_unavailable return Boolean
   is
      use type IC.int;
   begin
      return (fetch_h.fetchLastErrCode = fetch_h.FETCH_UNAVAIL);
   end last_fetch_unavailable;


   --------------------------------------------------------------------
   --  get_last_fetch_error
   --------------------------------------------------------------------
   function get_last_fetch_error return String
   is
   begin
      return IC.To_Ada (fetch_h.fetchLastErrString);
   end get_last_fetch_error;


   --------------------------------------------------------------------
   --  set_fetch_timeout
   --------------------------------------------------------------------
   procedure set_fetch_timeout (timeout : Natural) is
   begin
      fetch_h.fetchTimeout := IC.int (timeout);
   end set_fetch_timeout;


   --------------------------------------------------------------------
   --  get_fetch_timeout
   --------------------------------------------------------------------
   function get_fetch_timeout return Natural is
   begin
      return Natural (fetch_h.fetchTimeout);
   end get_fetch_timeout;


   --------------------------------------------------------------------
   --  fx_error
   --------------------------------------------------------------------
   function fx_error (fstream : Fetch_Stream) return Boolean
   is
      use type IC.int;
      res : IC.int;
   begin
      res := fetch_h.es_ferror (fstream.estream);
      return (res /= IC.int (0));
   end fx_error;


   --------------------------------------------------------------------
   --  fx_close
   --------------------------------------------------------------------
   procedure fx_close (fstream : in out Fetch_Stream)
   is
      use type IC.int;
      res : IC.int;
   begin
      if fstream.active then
         res := fetch_h.es_fclose (fstream.estream);
         if res /= IC.int (0) then
            raise EStream_Error;
         end if;
         fstream.active := False;
      end if;
   end fx_close;


   --------------------------------------------------------------------
   --  fx_print
   --------------------------------------------------------------------
   procedure fx_print (fstream : Fetch_Stream; message : String)
   is
      use type IC.int;
      res : IC.int;
      c_msg : ICS.chars_ptr;
   begin
      c_msg := ICS.New_String (message);
      res := fetch_h.es_fprintf (fstream.estream, c_msg);
      ICS.Free (c_msg);
      if res < IC.int (-1) then
         raise EStream_Error;
      end if;
   end fx_print;


   --------------------------------------------------------------------
   --  fx_getline
   --------------------------------------------------------------------
   function fx_getline (fstream : Fetch_Stream; finished : out Boolean) return String
   is
      use type IC.Extensions.long_long;
      chars_written : IC.Extensions.long_long;
      line          : ICS.chars_ptr;
      linecapp      : aliased IC.size_t := IC.size_t (0);
   begin
      chars_written := fetch_h.es_getline (line'Address, linecapp'Access, fstream.estream);
      if chars_written < IC.Extensions.long_long (0) then
         finished := True;
         return "";
      else
         finished := False;
         return ICS.Value (line);
      end if;
   end fx_getline;


   --------------------------------------------------------------------
   --  fx_fread
   --------------------------------------------------------------------
   function fx_fread
     (fstream       : Fetch_Stream;
      number_chunks : Positive;
      chunk_size    : Positive;
      chunks_read   : out Natural) return String
   is
      use type IC.size_t;
      objects_read : IC.size_t;
      arg2         : constant IC.size_t := IC.size_t (chunk_size);
      arg3         : constant IC.size_t := IC.size_t (number_chunks);
      buffer       : aliased IC.char_array (1 .. arg2 * arg3);
   begin
      objects_read := fetch_h.es_fread (buffer (buffer'First)'Access, arg2, arg3, fstream.estream);
      chunks_read := Natural (objects_read);
      declare
         result_size : constant Natural := chunks_read * chunk_size;
         result : String (1 .. result_size);
         index  : IC.size_t := IC.size_t (1);
      begin
         for x in result'Range loop
            result (x) := Character (buffer (index));
            index := index + 1;
         end loop;
         return result;
      end;
   end fx_fread;


   --------------------------------------------------------------------
   --  url_is_valid
   --------------------------------------------------------------------
   function url_is_valid (url_components : URL_Component_Set) return Boolean is
   begin
      return url_components.valid;
   end url_is_valid;


   --------------------------------------------------------------------
   --  parse_url
   --------------------------------------------------------------------
   function parse_url (url : String) return URL_Component_Set
   is
      url_components : URL_Component_Set;
      arg1           : ICS.chars_ptr;
   begin
      arg1 := ICS.New_String (url);
      url_components.components := fetch_h.fetchParseURL (arg1);
      url_components.orig_doc   := url_components.components.doc;
      ICS.Free (arg1);
      url_components.valid := (url_components.components /= null);
      return url_components;
   end parse_url;


   --------------------------------------------------------------------
   --  free_url
   --------------------------------------------------------------------
   procedure free_url (url_components : URL_Component_Set) is
   begin
      url_components.components.doc := url_components.orig_doc;
      fetch_h.fetchFreeURL (url_components.components);
   end free_url;


   --------------------------------------------------------------------
   --  provide_IMS_timestamp
   --------------------------------------------------------------------
   procedure provide_IMS_timestamp
     (timestamp : Core.Unix.T_epochtime;
      url_components : in out URL_Component_Set)
   is
      c_timestamp : Interfaces.Integer_64 := Interfaces.Integer_64 (timestamp);
   begin
      url_components.components.ims_time := c_timestamp;
   end provide_IMS_timestamp;


   --------------------------------------------------------------------
   --  provide_host_information
   --------------------------------------------------------------------
   procedure provide_host_information
     (host : String;
      port : Natural;
      url_components : in out URL_Component_Set)
   is
      use type IC.size_t;
      index : IC.size_t := url_components.components.host'First;
   begin
      url_components.components.port := IC.int (port);
      url_components.components.host := (others => IC.char'Val (0));
      for x in host'Range loop
         url_components.components.host (index) := IC.char'Val (Character'Pos (host (x)));
         index := index + 1;
      end loop;
   end provide_host_information;


   --------------------------------------------------------------------
   --  provide_scheme
   --------------------------------------------------------------------
   procedure provide_scheme
     (scheme : String;
      url_components : in out URL_Component_Set)
   is
      use type IC.size_t;
      index : IC.size_t := url_components.components.scheme'First;
   begin
      url_components.components.scheme := (others => IC.char'Val (0));
      for x in scheme'Range loop
         url_components.components.scheme (index) := IC.char'Val (Character'Pos (scheme (x)));
         index := index + 1;
      end loop;
   end provide_scheme;


   --------------------------------------------------------------------
   --  provide_doc
   --------------------------------------------------------------------
   procedure provide_doc
     (doc : String;
      holder : ICS.char_array_access;
      url_components : in out URL_Component_Set)
   is
      use type IC.size_t;
      index : IC.size_t := holder.all'First;
   begin
      holder.all := (others => IC.char'Val (0));
      for x in doc'Range loop
         holder.all (index) := IC.char'Val (Character'Pos (doc (x)));
         index := index + 1;
      end loop;
      url_components.components.doc := ICS.To_Chars_Ptr (holder);
   end provide_doc;


   --------------------------------------------------------------------
   --  provide_offset
   --------------------------------------------------------------------
   procedure provide_offset
     (offset : Core.Unix.T_filesize;
      url_components : in out URL_Component_Set)
   is
      c_offset : IC.int := IC.int (offset);
   begin
      url_components.components.offset := c_offset;
   end provide_offset;


   --------------------------------------------------------------------
   --  url_scheme
   --------------------------------------------------------------------
   function url_scheme (url_components : URL_Component_Set) return String is
   begin
      return IC.To_Ada (url_components.components.scheme);
   end url_scheme;


   --------------------------------------------------------------------
   --  url_user
   --------------------------------------------------------------------
   function url_user (url_components : URL_Component_Set) return String is
   begin
      return IC.To_Ada (url_components.components.user);
   end url_user;


   --------------------------------------------------------------------
   --  url_host
   --------------------------------------------------------------------
   function url_host (url_components : URL_Component_Set) return String is
   begin
      return IC.To_Ada (url_components.components.host);
   end url_host;


   --------------------------------------------------------------------
   --  url_user_at_host
   --------------------------------------------------------------------
   function url_user_at_host (url_components : URL_Component_Set) return String
   is
      user : String := url_user (url_components);
      host : String := url_host (url_components);
   begin
      if user = "" then
         return host;
      else
         return user & "@" & host;
      end if;
   end url_user_at_host;


   --------------------------------------------------------------------
   --  stream_is_active
   --------------------------------------------------------------------
   function stream_is_active (fstream : Fetch_Stream) return Boolean is
   begin
      return fstream.active;
   end stream_is_active;


   --------------------------------------------------------------------
   --  url_pwd
   --------------------------------------------------------------------
   function url_pwd (url_components : URL_Component_Set) return String is
   begin
      return IC.To_Ada (url_components.components.pwd);
   end url_pwd;


   --------------------------------------------------------------------
   --  url_doc
   --------------------------------------------------------------------
   function url_doc (url_components : URL_Component_Set) return String is
   begin
      return ICS.Value (url_components.components.doc);
   end url_doc;


   --------------------------------------------------------------------
   --  url_ims_time
   --------------------------------------------------------------------
   function url_ims_time (url_components : URL_Component_Set) return Core.Unix.T_epochtime
   is
      use type Interfaces.Integer_64;
   begin
      if url_components.components.ims_time < 0 then
         return 0;
      else
         return Core.Unix.T_epochtime (url_components.components.ims_time);
      end if;
   end url_ims_time;


   --------------------------------------------------------------------
   --  url_port
   --------------------------------------------------------------------
   function url_port (url_components : URL_Component_Set) return Natural
   is
   begin
      return Natural (url_components.components.port);
   end url_port;


   --------------------------------------------------------------------
   --  url_offset
   --------------------------------------------------------------------
   function url_offset (url_components : URL_Component_Set) return Integer is
   begin
      return Integer (url_components.components.offset);
   end url_offset;


   --------------------------------------------------------------------
   --  url_length
   --------------------------------------------------------------------
   function url_length (url_components : URL_Component_Set) return Natural is
   begin
      return Natural (url_components.components.length);
   end url_length;


   --------------------------------------------------------------------
   --  url_netrcfd
   --------------------------------------------------------------------
   function url_netrcfd (url_components : URL_Component_Set) return Core.Unix.File_Descriptor is
   begin
      return Core.Unix.File_Descriptor (url_components.components.netrcfd);
   end url_netrcfd;


   --------------------------------------------------------------------
   --  open_cookie
   --------------------------------------------------------------------
   function open_cookie
     (cookie    : System.Address;
      functions : fetch_h.es_cookie_io_functions_t) return Fetch_Stream
   is
      use type fetch_h.Extended_Stream;
      es_mode : ICS.chars_ptr;
      result  : Fetch_Stream;
   begin
      es_mode := ICS.New_String ("a+");
      result.estream := fetch_h.es_fopencookie (cookie    => cookie,
                                                mode      => es_mode,
                                                functions => functions);
      if result.estream /= null then
         result.active := True;
      end if;

      ICS.Free (es_mode);
      return result;
   end open_cookie;


   --------------------------------------------------------------------
   --  fx_GetURL
   --------------------------------------------------------------------
   function fx_GetURL (URL, flags : String) return Fetch_Stream
   is
      use type fetch_h.Extended_Stream;
      arg1 : ICS.chars_ptr;
      arg2 : ICS.chars_ptr;
      result : Fetch_Stream;
   begin
      arg1 := ICS.New_String (URL);
      arg2 := ICS.New_String (flags);

      result.estream := fetch_h.fetchGetURL (arg1, arg2);
      if result.estream /= null then
         result.active := True;
      end if;

      ICS.Free (arg1);
      ICS.Free (arg2);

      return result;
   end fx_GetURL;


   --------------------------------------------------------------------
   --  fx_XGet
   --------------------------------------------------------------------
   function fx_XGet
     (url_components : in out URL_Component_Set;
      flags          : String) return Fetch_Stream
   is
      use type fetch_h.Extended_Stream;
      arg3   : ICS.chars_ptr;
      result : Fetch_Stream;
   begin
      arg3 := ICS.New_String (flags);
      result.estream := fetch_h.fetchXGet (arg1 => url_components.components,
                                           arg2 => url_components.status'Access,
                                           arg3 => arg3);
      if result.estream /= null then
         result.active := True;
      end if;

      ICS.Free (arg3);
      return result;
   end fx_XGet;


   --------------------------------------------------------------------
   --  get_file_modification_time
   --------------------------------------------------------------------
   function get_file_modification_time
     (url_components : URL_Component_Set) return Core.Unix.T_epochtime is
   begin
      return Core.Unix.T_epochtime (url_components.status.mtime);
   end get_file_modification_time;


   --------------------------------------------------------------------
   --  get_fetched_file_size
   --------------------------------------------------------------------
   function get_fetched_file_size
     (url_components : URL_Component_Set) return Core.Unix.T_filesize is
   begin
      return Core.Unix.T_filesize (url_components.status.size);
   end get_fetched_file_size;

end Libfetch;
