--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package libarchive_h is

   pragma Preelaborate;

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
   package ICX renames Interfaces.C.Extensions;


   ------------------------
   --  Type Definitions  --
   ------------------------

   type arc64 is new Interfaces.Integer_64;

   type archive is limited private;
   type archive_entry is limited private;

   type archive_Access is access all archive;
   pragma Convention (C, archive_Access);

   type archive_entry_Access is access all archive_entry;
   pragma Convention (C, archive_entry_Access);

   type AEA_Access is access all archive_entry_Access;

   ARCHIVE_OK     : constant :=   0;  --  Operational was successful
   ARCHIVE_EOF    : constant :=   1;  --  Found end of archive.
   ARCHIVE_RETRY  : constant := -10;  --  Retry might succeed
   ARCHIVE_WARN   : constant := -20;  --  Partial success
   ARCHIVE_FAILED : constant := -25;  --  Current operation cannot complete.
   ARCHIVE_FATAL  : constant := -30;  --  No more operations is possible

   -------------------
   --  <archive.h>  --
   -------------------

   function archive_read_support_filter_all (arc_handle : archive_Access) return IC.int;
   pragma Import (C, archive_read_support_filter_all);

   function archive_read_support_filter_zstd (arc_handle : archive_Access) return IC.int;
   pragma Import (C, archive_read_support_filter_zstd);

   function archive_read_support_filter_none (arc_handle : archive_Access) return IC.int;
   pragma Import (C, archive_read_support_filter_none);

   function archive_read_support_format_all (arc_handle : archive_Access) return IC.int;
   pragma Import (C, archive_read_support_format_all);

   function archive_read_support_format_tar (arc_handle : archive_Access) return IC.int;
   pragma Import (C, archive_read_support_format_tar);

   function archive_read_new return archive_Access;
   pragma Import (C, archive_read_new);

   function archive_read_open_filename
     (arc_handle : archive_Access;
      filename   : ICS.chars_ptr;
      block_size : IC.size_t) return IC.int;
   pragma Import (C, archive_read_open_filename);

   function archive_read_open_fd
     (arc_handle : archive_Access;
      fd         : IC.int;
      block_size : IC.size_t) return IC.int;
   pragma Import (C, archive_read_open_fd);

   function archive_read_next_header
     (arc_handle : archive_Access;
      header     : AEA_Access) return IC.int;
   pragma Import (C, archive_read_next_header);

   function archive_read_data
     (arc_handle : archive_Access;
      data       : ICX.void_ptr;
      size       : IC.size_t) return IC.long;
   pragma Import (C, archive_read_data);

   function archive_read_close (arc_handle : archive_Access) return IC.int;
   pragma Import (C, archive_read_close);

   function archive_read_free (arc_handle : archive_Access) return IC.int;
   pragma Import (C, archive_read_free);

   -------------------------
   --  <archive_entry.h>  --
   -------------------------

   function archive_entry_pathname (header : archive_entry_Access) return ICS.chars_ptr;
   pragma Import (C, archive_entry_pathname);

   function archive_entry_size (header : archive_entry_Access) return arc64;
   pragma Import (C, archive_entry_size);

private

   type archive       is limited null record;
   type archive_entry is limited null record;

end libarchive_h;
