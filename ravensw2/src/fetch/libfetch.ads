--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with fetch_h;
use fetch_h;

package Libfetch is

   EStream_Error : exception;

   --  close extended stream, raise EStream_Error on failure
   procedure fx_close (estream : Extended_Stream);

   --  push message to extended stream, raise EStream_Error on failure
   procedure fx_print (estream : Extended_Stream; message : String);

   --  Set libfetch.fetchTimeout
   procedure set_fetch_timeout (timeout : Natural);

   --  Returns True if an error occurred on the extended stream
   function fx_error (estream : Extended_Stream) return Boolean;

   --  Returns next string delimited by unix newline.  The result does NOT include
   --  the newline delimiters.  Finished is true on EOF or error
   function fx_getline (estream : Extended_Stream; finished : out Boolean) return String;

   --  Given a number of objects of a given size to read, return the concatenated
   --  data as a string.  When successful, chunks read will equal number_chunks.
   --  Reasons for chunks_read being less include reaching End of File or encountering an error
   function fx_read
     (estream : Extended_Stream;
      number_chunks : Positive;
      chunk_size    : Positive;
      chunks_read   : out Natural) return String;

   --  Returns last error message from libfetch
   function get_last_fetch_error return String;

   --  Return True if fetchLastErrCode == FETCH_OK
   function last_fetch_ok return Boolean;

   --  Return True if fetchLastErrCode == FETCH_UNAVAIL
   function last_fetch_unavailable return Boolean;

end Libfetch;
