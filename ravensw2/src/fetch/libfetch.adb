--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Interfaces.C.Strings;

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
      return (fetchLastErrCode = FETCH_OK);
   end last_fetch_ok;


   --------------------------------------------------------------------
   --  last_fetch_unavailable
   --------------------------------------------------------------------
   function last_fetch_unavailable return Boolean
   is
      use type IC.int;
   begin
      return (fetchLastErrCode = FETCH_UNAVAIL);
   end last_fetch_unavailable;


   --------------------------------------------------------------------
   --  get_last_fetch_error
   --------------------------------------------------------------------
   function get_last_fetch_error return String
   is
   begin
      return IC.To_Ada (fetchLastErrString);
   end get_last_fetch_error;


   --------------------------------------------------------------------
   --  set_fetch_timeout
   --------------------------------------------------------------------
   procedure set_fetch_timeout (timeout : Natural) is
   begin
      fetchTimeout := IC.int (timeout);
   end set_fetch_timeout;


   --------------------------------------------------------------------
   --  fx_error
   --------------------------------------------------------------------
   function fx_error (estream : Extended_Stream) return Boolean
   is
      use type IC.int;
      res : IC.int;
   begin
      res := es_ferror (estream);
      return (res /= IC.int (0));
   end fx_error;


   --------------------------------------------------------------------
   --  fx_close
   --------------------------------------------------------------------
   procedure fx_close (estream : Extended_Stream)
   is
      use type IC.int;
      res : IC.int;
   begin
      res := es_fclose (estream);
      if res /= IC.int (0) then
         raise EStream_Error;
      end if;
   end fx_close;


   --------------------------------------------------------------------
   --  fx_print
   --------------------------------------------------------------------
   procedure fx_print (estream : Extended_Stream; message : String)
   is
      use type IC.int;
      res : IC.int;
      c_msg : ICS.chars_ptr;
   begin
      c_msg := ICS.New_String (message);
      res := es_fprintf (estream, c_msg);
      ICS.Free (c_msg);
      if res < IC.int (-1) then
         raise EStream_Error;
      end if;
   end fx_print;


   --------------------------------------------------------------------
   --  fx_getline
   --------------------------------------------------------------------
   function fx_getline (estream : Extended_Stream; finished : out Boolean) return String
   is
      use type IC.long;
      chars_written : IC.long;
      line          : ICS.chars_ptr;
      linecapp      : aliased IC.size_t := IC.size_t (0);
   begin
      chars_written := es_getline (line'Address, linecapp'Access, estream);
      if chars_written < IC.long (0) then
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
     (estream : Extended_Stream;
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
      objects_read := es_fread (buffer (buffer'First)'Access, arg2, arg3, estream);
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


end Libfetch;
