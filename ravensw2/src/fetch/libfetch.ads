--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Unix;
private with fetch_h;

package Libfetch is

   EStream_Error : exception;

   type URL_Component_Set is private;
   type Fetch_Stream is private;

   --  close extended stream, raise EStream_Error on failure
   procedure fx_close (fstream : in out Fetch_Stream);

   --  push message to extended stream, raise EStream_Error on failure
   procedure fx_print (fstream : Fetch_Stream; message : String);

   --  Set libfetch.fetchTimeout
   procedure set_fetch_timeout (timeout : Natural);

   --  Returns True if an error occurred on the extended stream
   function fx_error (fstream : Fetch_Stream) return Boolean;

   --  Returns next string delimited by unix newline.  The result does NOT include
   --  the newline delimiters.  Finished is true on EOF or error
   function fx_getline (fstream : Fetch_Stream; finished : out Boolean) return String;

   --  Given a number of objects of a given size to read, return the concatenated
   --  data as a string.  When successful, chunks read will equal number_chunks.
   --  Reasons for chunks_read being less include reaching End of File or encountering an error
   function fx_read
     (fstream       : Fetch_Stream;
      number_chunks : Positive;
      chunk_size    : Positive;
      chunks_read   : out Natural) return String;

   --  Returns last error message from libfetch
   function get_last_fetch_error return String;

   --  Return True if fetchLastErrCode == FETCH_OK
   function last_fetch_ok return Boolean;

   --  Return True if fetchLastErrCode == FETCH_UNAVAIL
   function last_fetch_unavailable return Boolean;

   --  breaks down url into its components
   function parse_url (url : String) return URL_Component_Set;

   --  Returns True if url parsing was succesful
   function url_is_valid (url_components : URL_Component_Set) return Boolean;

   --  If timestamp > 0 then set the url components with it
   procedure provide_last_timestamp
     (timestamp : Core.Unix.T_epochtime;
      url_components : in out URL_Component_Set);

   --  Return scheme component of url
   function url_scheme (url_components : URL_Component_Set) return String;

private

   type URL_Component_Set is
      record
         valid      : Boolean;
         components : access fetch_h.url;
      end record;

   type Fetch_Stream is
      record
         estream : fetch_h.Extended_Stream;
         active  : Boolean;
      end record;

end Libfetch;
