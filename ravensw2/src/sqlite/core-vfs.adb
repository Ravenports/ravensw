--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Context;
with Core.Strings; use Core.Strings;


package body Core.VFS is

   --------------------------------------------------------------------
   --  dbdir_open
   --------------------------------------------------------------------
   function dbdir_open (path : ICS.chars_ptr; flags : IC.int; mode : IC.int) return IC.int
   is
      dfd      : Unix.File_Descriptor := Context.reveal_db_directory_fd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      fd       : IC.int;
   begin
      newpath := ICS.New_String (basename);
      fd := Unix.C_Openat_Stock (dirfd => IC.int (dfd),
                                 path  => newpath,
                                 flags => flags,
                                 mode  => mode);
      ICS.Free (newpath);
      return fd;
   end dbdir_open;


   --------------------------------------------------------------------
   --  dbdir_access
   --------------------------------------------------------------------
   function dbdir_access (path : ICS.chars_ptr; mode : IC.int) return IC.int
   is
      dfd      : Unix.File_Descriptor := Context.reveal_db_directory_fd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      res      : IC.int;
   begin
      newpath := ICS.New_String (basename);
      res     := Unix.C_faccessat (dfd  => IC.int (dfd),
                                   path => newpath,
                                   mode => mode,
                                   flag => 0);
      ICS.Free (newpath);
      return res;
   end dbdir_access;


   --------------------------------------------------------------------
   --  dbdir_unlink
   --------------------------------------------------------------------
   function dbdir_unlink (path : ICS.chars_ptr) return IC.int
   is
      dfd      : Unix.File_Descriptor := Context.reveal_db_directory_fd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      res      : IC.int;
   begin
      newpath := ICS.New_String (basename);
      res     := Unix.C_unlinkat (dfd  => dfd,
                                  path => newpath,
                                  remove_dir => 0);
      ICS.Free (newpath);
      return res;
   end dbdir_unlink;


   --------------------------------------------------------------------
   --  dbdir_stat
   --------------------------------------------------------------------
   function dbdir_stat (path : ICS.chars_ptr; sb : Unix.struct_stat_Access) return IC.int
   is
      dfd      : Unix.File_Descriptor := Context.reveal_db_directory_fd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      res      : IC.int;
   begin
      newpath := ICS.New_String (basename);
      res     := Unix.C_fstatat (dfd  => IC.int (dfd),
                                 path => newpath,
                                 sb   => sb,
                                 flag => 0);
      ICS.Free (newpath);
      return res;
   end dbdir_stat;


   --------------------------------------------------------------------
   --  dbdir_lstat
   --------------------------------------------------------------------
   function dbdir_lstat (path : ICS.chars_ptr; sb : Unix.struct_stat_Access) return IC.int
   is
      dfd      : Unix.File_Descriptor := Context.reveal_db_directory_fd;
      basename : String := tail (ICS.Value (path), "/");
   begin
      if Unix.lstatat (dfd, basename, sb) then
         return IC.int (0);
      else
         return IC.int (-1);
      end if;
   end dbdir_lstat;


   --------------------------------------------------------------------
   --  dbdir_mkdir
   --------------------------------------------------------------------
   function dbdir_mkdir (path : ICS.chars_ptr; mode : IC.int) return IC.int
   is
      dfd      : Unix.File_Descriptor := Context.reveal_db_directory_fd;
      basename : String := tail (ICS.Value (path), "/");
      newpath  : ICS.chars_ptr;
      res      : IC.int;
   begin
      newpath := ICS.New_String (basename);
      res := Unix.C_mkdirat (dfd  => IC.int (dfd),
                             path => newpath,
                             mode => IC.int (mode));
      ICS.Free (newpath);
      return res;
   end dbdir_mkdir;


end Core.VFS;
