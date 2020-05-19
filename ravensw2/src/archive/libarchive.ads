--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with libarchive_h; use libarchive_h;
with Core.Unix;

package libarchive is

   archive_error : exception;

   type arc64 is mod 2 ** 64;

   --  thick archive_read_close
   procedure read_close (arc : archive_Access);

   --  thick archive_read_free
   procedure read_free (arc : archive_Access);

   --  thick archive_read_support_filter_all
   procedure read_support_filter_all (arc : archive_Access);

   --  thick archive_read_support_format_tar
   procedure read_support_format_tar (arc : archive_Access);

   --  thick archive_read_open_filename
   procedure read_open_filename
     (arc        : archive_Access;
      path       : String;
      block_size : Positive);

   --  thick archive_read_open_fd
   procedure read_open_fd
     (arc        : archive_Access;
      fd         : Core.Unix.File_Descriptor;
      block_size : Positive);

   --  thick archive_entry_pathname
   function entry_pathname (arcent : archive_entry_Access) return String;

   --  thick archive_entry_size
   function entry_size (arcent : archive_entry_Access) return arc64;

   --  thick archive_read_data
   function read_data (arc : archive_Access; chunk_size : arc64) return String;

   --  thick archive_error_string
   function error_string (arc : archive_Access) return String;

   --  thick archive_read_next_header
   function read_next_header
     (arc    : archive_Access;
      arcent : archive_entry_Access;
      error  : out Boolean;
      final  : out Boolean) return Boolean;

   --  thick archive_read_into_fd
   function read_data_into_file_descriptor
     (arc : archive_Access;
      fd  : Core.Unix.File_Descriptor) return Boolean;

end libarchive;
