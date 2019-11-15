--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with libarchive_h; use libarchive_h;
with Core.Unix;

package libarchive is

   archive_error : exception;

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
      chunk_size : Positive);

   --  thick archive_read_open_fd
   procedure read_open_fd
     (arc        : archive_Access;
      fd         : Core.Unix.File_Descriptor;
      chunk_size : Positive);

end libarchive;
