--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

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
      chunk_size : Positive)
   is
      --  archive_read_open_filename() treats a path of NULL as
      --  meaning "read from stdin," but we want this behaviour if
      --  path is exactly "-". In the unlikely event of wanting to
      --  read an on-disk file called "-", just say "./-" or some
      --  other leading path. */
      read_from_stdin : Boolean := (path = "-");
   begin
      if read_from_stdin then
         if archive_read_open_filename (arc, ICS.Null_Ptr, IC.size_t (chunk_size)) /= ARCHIVE_OK
         then
            raise archive_error;
         end if;
      else
         declare
            c_path  : ICS.chars_ptr;
            res     : archive_result;
         begin
            c_path := ICS.New_String (path);
            res := archive_read_open_filename (arc, c_path, IC.size_t (chunk_size));
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
      chunk_size : Positive)
   is
   begin
      if archive_read_open_fd (arc_handle => arc,
                               fd         => IC.int (fd),
                               block_size => IC.size_t (chunk_size)) /= ARCHIVE_OK
      then
         raise archive_error;
      end if;
   end read_open_fd;

end libarchive;
