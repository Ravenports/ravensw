--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with System;
with Core.Unix;
with fetch_h;

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

   --  Retrieve libfetch.fetchTimeout
   function get_fetch_timeout return Natural;

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

   --  Set the If-Modified-Since timestamp
   procedure provide_IMS_timestamp
     (timestamp : Core.Unix.T_epochtime;
      url_components : in out URL_Component_Set);

   --  Return scheme component of url
   function url_scheme (url_components : URL_Component_Set) return String;

   --  Return doc commonent of url
   function url_doc (url_components : URL_Component_Set) return String;

   --  Return If-Modified-Since time of url
   function url_ims_time (url_components : URL_Component_Set) return Core.Unix.T_epochtime;

   --  Return port component of url
   function url_port (url_components : URL_Component_Set) return Natural;

   --  Return user component of url
   function url_user (url_components : URL_Component_Set) return String;

   --  Return host component of url
   function url_host (url_components : URL_Component_Set) return String;

   --  Return pwd component of url
   function url_pwd (url_components : URL_Component_Set) return String;

   --  Return offset component of url
   function url_offset (url_components : URL_Component_Set) return Integer;

   --  Return length component of url
   function url_length (url_components : URL_Component_Set) return Natural;

   --  Return netrcfd component of url
   function url_netrcfd (url_components : URL_Component_Set) return Core.Unix.File_Descriptor;

   --  Return True if extended stream is connected
   function stream_is_active (fstream : Fetch_Stream) return Boolean;

   function open_cookie
     (cookie    : System.Address;
      functions : fetch_h.es_cookie_io_functions_t) return Fetch_Stream;

   function fx_GetURL (URL, flags : String) return Fetch_Stream;

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
