--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Interfaces.C.Strings;
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
   --  fx_read
   --------------------------------------------------------------------
   function fx_read
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
   end fx_read;


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
      url_components.components  := fetch_h.fetchParseURL (arg1);
      ICS.Free (arg1);
      url_components.valid := (url_components.components /= null);
      return url_components;
   end parse_url;


   --------------------------------------------------------------------
      --  provide_last_timestamp
   --------------------------------------------------------------------
   procedure provide_last_timestamp
     (timestamp : Core.Unix.T_epochtime;
      url_components : in out URL_Component_Set)
   is
      use type IC.long;
      c_timestamp : IC.long := IC.long (timestamp);
   begin
      if c_timestamp > 0 then
         url_components.components.ims_time := c_timestamp;
      end if;
   end provide_last_timestamp;


   --------------------------------------------------------------------
      --  url_scheme
   --------------------------------------------------------------------
   function url_scheme (url_components : URL_Component_Set) return String is
   begin
      return IC.To_Ada (url_components.components.scheme);
   end url_scheme;

end Libfetch;
