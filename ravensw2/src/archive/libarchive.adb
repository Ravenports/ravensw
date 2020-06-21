--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with libarchive_h;

package body libarchive is

   use type archive_result;

   --------------------------------------------------------------------
   --  read_close
   --------------------------------------------------------------------
   procedure read_close (arc : archive_Access) is
   begin
      if archive_read_close (arc) /= ARCHIVE_OK then
         raise archive_error;
      end if;
   end read_close;


   --------------------------------------------------------------------
   --  archive_read_free
   --------------------------------------------------------------------
   procedure read_free (arc : libarchive_h.archive_Access) is
   begin
      if archive_read_free (arc) /= ARCHIVE_OK then
         raise archive_error;
      end if;
   end read_free;


   --------------------------------------------------------------------
   --  read_support_filter_all
   --------------------------------------------------------------------
   procedure read_support_filter_all (arc : archive_Access) is
   begin
      if archive_read_support_filter_all (arc) /= ARCHIVE_OK then
         raise archive_error;
      end if;
   end read_support_filter_all;


   --------------------------------------------------------------------
   --  read_support_format_tar
   --------------------------------------------------------------------
   procedure read_support_format_tar (arc : archive_Access) is
   begin
      if archive_read_support_format_tar (arc) /= ARCHIVE_OK then
         raise archive_error;
      end if;
   end read_support_format_tar;


   --------------------------------------------------------------------
   --  read_open_filename
   --------------------------------------------------------------------
   procedure read_open_filename
     (arc        : archive_Access;
      path       : String;
      block_size : Positive)
   is
      --  archive_read_open_filename() treats a path of NULL as
      --  meaning "read from stdin," but we want this behaviour if
      --  path is exactly "-". In the unlikely event of wanting to
      --  read an on-disk file called "-", just say "./-" or some
      --  other leading path. */
      read_from_stdin : Boolean := (path = "-");
   begin
      if read_from_stdin then
         if archive_read_open_filename (arc, ICS.Null_Ptr, IC.size_t (block_size)) /= ARCHIVE_OK
         then
            raise archive_error;
         end if;
      else
         declare
            c_path  : ICS.chars_ptr;
            res     : archive_result;
         begin
            c_path := ICS.New_String (path);
            res := archive_read_open_filename (arc, c_path, IC.size_t (block_size));
            ICS.Free (c_path);
            if res /= ARCHIVE_OK then
               raise archive_error;
            end if;
         end;
      end if;
   end read_open_filename;


   --------------------------------------------------------------------
   --  read_open_fd
   --------------------------------------------------------------------
   procedure read_open_fd
     (arc        : archive_Access;
      fd         : Core.Unix.File_Descriptor;
      block_size : Positive) is
   begin
      if archive_read_open_fd (arc_handle => arc,
                               fd         => IC.int (fd),
                               block_size => IC.size_t (block_size)) /= ARCHIVE_OK
      then
         raise archive_error;
      end if;
   end read_open_fd;


   --------------------------------------------------------------------
   --  entry_pathname
   --------------------------------------------------------------------
   function entry_pathname (arcent : archive_entry_Access) return String
   is
      use type ICS.chars_ptr;
      pathname : ICS.chars_ptr := archive_entry_pathname (arcent);
   begin
      if pathname = ICS.Null_Ptr then
         return "";
      else
         return ICS.Value (pathname);
      end if;
   end entry_pathname;


   --------------------------------------------------------------------
   --  entry_size
   --------------------------------------------------------------------
   function entry_size (arcent : archive_entry_Access) return arc64 is
   begin
      return arc64 (archive_entry_size (arcent));
   end entry_size;


   --------------------------------------------------------------------
   --  read_data
   --------------------------------------------------------------------
   function read_data (arc : archive_Access; chunk_size : arc64) return String
   is
      data       : String (1 .. Integer (chunk_size)) := (others => ' ');
      buffer     : array (data'Range) of aliased IC.unsigned_char;
      bytes_read : IC.long;
   begin
      bytes_read := archive_read_data (arc_handle => arc,
                                       data       => buffer (buffer'First)'Access,
                                       size       => IC.size_t (chunk_size));
      if Integer (bytes_read) < 0 then
         --  some kind of error encountered if this function is truly like read(2)
         raise archive_error;
      elsif Integer (bytes_read) = 0 then
         --  END OF FILE
         return "";
      else
         for x in 1 .. Integer (bytes_read) loop
            data (x) := Character'Val (buffer (x));
         end loop;
         return data (1 .. Integer (bytes_read));
      end if;
   end read_data;


   --------------------------------------------------------------------
   --  error_string
   --------------------------------------------------------------------
   function error_string (arc : archive_Access) return String is
   begin
      return ICS.Value (archive_error_string (arc));
   end error_string;


   --------------------------------------------------------------------
   --  read_next_header
   --------------------------------------------------------------------
   function read_next_header
     (arc    : archive_Access;
      arcent : AEA_Access;
      error  : out Boolean;
      final  : out Boolean) return Boolean
   is
      result : archive_result;
   begin
      result := archive_read_next_header (arc, arcent);
      final  := (result = ARCHIVE_EOF);
      error  := (result /= ARCHIVE_OK) and then (result /= ARCHIVE_EOF);
      return (result = ARCHIVE_OK);
   end read_next_header;


   --------------------------------------------------------------------
   --  read_data_into_file_descriptor
   --------------------------------------------------------------------
   function read_data_into_file_descriptor
     (arc : archive_Access;
      fd  : Core.Unix.File_Descriptor) return Boolean
   is
      result : archive_result;
   begin
      result := archive_read_data_into_fd (arc, IC.int (fd));
      return (result = ARCHIVE_OK);
   end read_data_into_file_descriptor;

end libarchive;
