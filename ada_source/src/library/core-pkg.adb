--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Event;
with Core.Utilities;
with Core.Strings;
with Core.Manifest;
with libarchive;

use Core.Strings;

package body Core.Pkg is

   --------------------------------------------------------------------
   --  repo_priority_less_than
   --------------------------------------------------------------------
   function repo_priority_less_than (A, B : T_repo_priority) return Boolean
   is
      --  Display 100 before 90, so it's reverse order (use greater than for "<")
   begin
      if A.priority = B.priority then
         return SU.">" (A.reponame, B.reponame);
      else
         return A.priority > B.priority;
      end if;
   end repo_priority_less_than;


   --------------------------------------------------------------------
   --  pkg_adddir_attr
   --------------------------------------------------------------------
   function pkg_adddir_attr
     (pkg_access : T_pkg_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : mode_t;
      fflags : T_dir_flags;
      check_duplicates : Boolean) return Pkg_Error_Type
   is
      D : T_pkg_dir;
      fullpath : Text;
   begin
      if not contains (path, "/") then
         Event.pkg_emit_error (SUS ("skipping useless directory: '" & USS (path) & "'"));
      end if;

      fullpath := SUS (Core.Utilities.pkg_absolutepath (USS (path), False));
      Event.pkg_debug (3, "Pkg: add new directory '" & USS (fullpath) & "'");
      if check_duplicates and then
        pkg_access.dirs.Contains (fullpath)
      then
         if context.developer_mode then
            Event.pkg_emit_error
              (SUS ("duplicate directory listing: " & USS (fullpath) &
                 ", fatal (developer mode)"));
            return (EPKG_FATAL);
         else
            Event.pkg_emit_error
              (SUS ("duplicate directory listing: " & USS (fullpath) & ", ignoring"));
            return (EPKG_FATAL);
         end if;
      end if;

      D.path   := fullpath;
      D.uname  := uname;
      D.gname  := gname;
      D.perm   := perm;
      D.fflags := fflags;

      pkg_access.dirs.Insert (fullpath, D);
      return EPKG_OK;
   end pkg_adddir_attr;


   --------------------------------------------------------------------
   --  pkg_addrdep
   --------------------------------------------------------------------
   function pkg_addrdep
     (pkg_access : T_pkg_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Pkg_Error_Type
   is
      D : T_pkg_dep;
   begin
      Event.pkg_debug
        (3, "Pkg: add a new reverse dependency origin: " & USS (origin) & ", name: " &
           USS (name));

      D.name    := name;
      D.origin  := origin;
      D.version := version;
      D.locked  := locked;
      D.uid     := name;

      pkg_access.rdepends.Prepend (D);
      return EPKG_OK;
   end pkg_addrdep;


   --------------------------------------------------------------------
   --  pkg_addconfig_file
   --------------------------------------------------------------------
   function pkg_addconfig_file
     (pkg_access : T_pkg_Access;
      path       : Text;
      content    : Text) return Pkg_Error_Type
   is
      CF : T_pkg_config_file;
      fullpath : Text;
   begin
      fullpath := SUS (Core.Utilities.pkg_absolutepath (USS (path), False));
      Event.pkg_debug (3, "Pkg: add new config file '" & USS (fullpath) & "'");
      if pkg_access.config_files.Contains (fullpath)
      then
         if context.developer_mode then
            Event.pkg_emit_error
              (SUS ("duplicate config file listing: " & USS (fullpath) &
                 ", fatal (developer mode)"));
            return (EPKG_FATAL);
         else
            Event.pkg_emit_error
              (SUS ("duplicate config file listing: " & USS (fullpath) & ", ignoring"));
            return (EPKG_FATAL);
         end if;
      end if;

      CF.path := fullpath;
      CF.content := content;

      pkg_access.config_files.Insert (fullpath, CF);
      return EPKG_OK;
   end pkg_addconfig_file;


   --------------------------------------------------------------------
   --  pkg_addfile_attr
   --------------------------------------------------------------------
   function pkg_addfile_attr
     (pkg_access : T_pkg_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : mode_t;
      fflags : T_dir_flags;
      sum    : Text;
      check_duplicates : Boolean) return Pkg_Error_Type
   is
      F : T_pkg_file;
      fullpath : Text;
   begin
      fullpath := SUS (Core.Utilities.pkg_absolutepath (USS (path), False));
      Event.pkg_debug (3, "Pkg: add new file '" & USS (fullpath) & "'");
      if check_duplicates and then
        pkg_access.dirs.Contains (fullpath)
      then
         if context.developer_mode then
            Event.pkg_emit_error
              (SUS ("duplicate file listing: " & USS (fullpath) & ", fatal (developer mode)"));
            return (EPKG_FATAL);
         else
            Event.pkg_emit_error
              (SUS ("duplicate file listing: " & USS (fullpath) & ", ignoring"));
            return (EPKG_FATAL);
         end if;
      end if;

      F.path   := fullpath;
      F.uname  := uname;
      F.gname  := gname;
      F.perm   := perm;
      F.fflags := fflags;
      F.sum    := sum;

      pkg_access.files.Append (F);
      return EPKG_OK;
   end pkg_addfile_attr;


   --------------------------------------------------------------------
   --  pkg_addoption
   --------------------------------------------------------------------
   function pkg_addoption
     (pkg_access : T_pkg_Access;
      key        : Text;
      value    : Text) return Pkg_Error_Type
   is
   begin
      Event.pkg_debug (2, "Pkg> adding options: " & USS (key) & " = " & USS (value));
      if pkg_access.options.Contains (key) then
         if context.developer_mode then
            Event.pkg_emit_error
              (SUS ("duplicate option listing: " & USS (key) & ", fatal (developer mode)"));
            return (EPKG_FATAL);
         else
            Event.pkg_emit_error
              (SUS ("duplicate option listing: " & USS (key) & ", ignoring"));
            return (EPKG_FATAL);
         end if;
      end if;

      pkg_access.options.Insert (key, value);
      return EPKG_OK;
   end pkg_addoption;


   --------------------------------------------------------------------
   --  pkg_addoption
   --------------------------------------------------------------------
   function pkg_adddep
     (pkg_access : T_pkg_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Pkg_Error_Type
   is
      D : T_pkg_dep;
   begin
      Event.pkg_debug
        (3, "Pkg: add a new dependency origin: " & USS (origin) & ", name: " & USS (name));

      if pkg_access.depends.Contains (name) then
         Event.pkg_emit_error
           (SUS (USS (pkg_access.name) & ": duplicate dependency listing: " & USS (name)));
         return EPKG_FATAL;
      end if;

      D.name    := name;
      D.origin  := origin;
      D.version := version;
      D.locked  := locked;
      D.uid     := name;

      pkg_access.depends.Insert (name, D);
      return EPKG_OK;
   end pkg_adddep;


   --------------------------------------------------------------------
   --  pkg_adddir
   --------------------------------------------------------------------
   function pkg_adddir
     (pkg_access : T_pkg_Access;
      path       : Text;
      check_duplicates : Boolean) return Pkg_Error_Type
   is
   begin
      return pkg_adddir_attr (pkg_access       => pkg_access,
                              path             => path,
                              uname            => blank,
                              gname            => blank,
                              perm             => 0,
                              fflags           => 0,
                              check_duplicates => check_duplicates);
   end pkg_adddir;


   --------------------------------------------------------------------
   --  pkg_addfile
   --------------------------------------------------------------------
   function pkg_addfile
     (pkg_access : T_pkg_Access;
      path       : Text;
      sum        : Text;
      check_duplicates : Boolean) return Pkg_Error_Type
   is
   begin
      return pkg_addfile_attr (pkg_access       => pkg_access,
                               path             => path,
                               uname            => blank,
                               gname            => blank,
                               perm             => 0,
                               fflags           => 0,
                               sum              => sum,
                               check_duplicates => check_duplicates);
   end pkg_addfile;


   --------------------------------------------------------------------
   --  pkg_addscript
   --------------------------------------------------------------------
   function pkg_addscript
     (pkg_access : T_pkg_Access;
      data       : Text;
      script     : pkg_script_type) return Pkg_Error_Type is
   begin
      pkg_access.scripts (script) := data;
      return EPKG_OK;
   end pkg_addscript;


   --------------------------------------------------------------------
   --  pkg_addstring
   --------------------------------------------------------------------
   function pkg_addstring
     (crate      : in out text_crate.Vector;
      data       : String;
      title      : String) return Pkg_Error_Type
   is
      datatxt : Text := SUS (data);
   begin
      if crate.Contains (datatxt) then
         if context.developer_mode then
            Event.pkg_emit_error
              (SUS ("duplicate " & title & " listing: " & data & ", fatal (developer mode)"));
            return EPKG_FATAL;
         else
            Event.pkg_emit_error
              (SUS ("duplicate " & title & " listing: " & data & ", ignoring"));
         end if;
      else
         crate.Append (datatxt);
      end if;
      return EPKG_OK;
   end pkg_addstring;


   --------------------------------------------------------------------
   --  pkg_addstring_silent_unique
   --------------------------------------------------------------------
   function pkg_addstring_silent_unique
     (crate      : in out text_crate.Vector;
      data       : String) return Pkg_Error_Type
   is
      datatxt : Text := SUS (data);
   begin
      if not crate.Contains (datatxt) then
         crate.Append (datatxt);
      end if;
      return EPKG_OK;
   end pkg_addstring_silent_unique;


   --------------------------------------------------------------------
   --  pkg_addshlib_required
   --------------------------------------------------------------------
   function pkg_addshlib_required
     (pkg_access : T_pkg_Access;
      data       : String) return Pkg_Error_Type
   is
      datatxt : Text := SUS (data);
   begin
      --  silently ignore duplicates in case of shlibs
      if not pkg_access.shlibs_reqd.Contains (datatxt) then
         pkg_access.shlibs_reqd.Append (datatxt);
         Event.pkg_debug (3, "added shlib deps for " & USS (pkg_access.name) & " on " & data);
      end if;
      return EPKG_OK;
   end pkg_addshlib_required;


   --------------------------------------------------------------------
   --  pkg_addshlib_provided
   --------------------------------------------------------------------
   function pkg_addshlib_provided
     (pkg_access : T_pkg_Access;
      data       : String) return Pkg_Error_Type
   is
      datatxt : Text := SUS (data);
   begin
      --  silently ignore files which are not starting with lib
      if leads (data, "lib") then
         --  silently ignore duplicates in case of shlibs
         if not pkg_access.shlibs_prov.Contains (datatxt) then
            pkg_access.shlibs_prov.Append (datatxt);
            Event.pkg_debug
              (3, "added shlib provided for " & USS (pkg_access.name) & " on " & data);
         end if;
      end if;
      return EPKG_OK;
   end pkg_addshlib_provided;


   --------------------------------------------------------------------
   --  pkg_addconflict
   --------------------------------------------------------------------
   function pkg_addconflict
     (pkg_access : T_pkg_Access;
      data       : String) return Pkg_Error_Type
   is
      unique_id : Text := SUS (data);
      conflict  : T_pkg_conflict;
   begin
      --  silently ignore duplicates in case of conflicts
      if not pkg_access.conflicts.Contains (unique_id) then
         conflict.uid := unique_id;  --  digest and type not set (should it be?)
         pkg_access.conflicts.Insert (unique_id, conflict);
         Event.pkg_debug
           (3, "add a new conflict origin: " & USS (pkg_access.uid) & ", with " & data);
      end if;
      return EPKG_OK;
   end pkg_addconflict;


   --------------------------------------------------------------------
   --  pkg_open
   --------------------------------------------------------------------
   function pkg_open
     (pkg_ptr : in out T_pkg_Access;
      path    : String;
      flags   : T_open_flags) return Pkg_Error_Type
   is
      arc     : libarchive_h.archive_Access;
      arcent  : libarchive_h.archive_entry_Access;
      retcode : Pkg_Error_Type;
   begin

      retcode := pkg_open2 (pkg_ptr => pkg_ptr,
                            arc     => arc,
                            arcent  => arcent,
                            path    => path,
                            flags   => flags,
                            fd      => Unix.not_connected);

      if retcode /= EPKG_OK and then
        retcode /= EPKG_END
      then
         return EPKG_FATAL;
      end if;

      return EPKG_OK;
   end pkg_open;


   --------------------------------------------------------------------
   --  pkg_open_fd
   --------------------------------------------------------------------
   function pkg_open_fd
     (pkg_ptr : in out T_pkg_Access;
      fd      : Unix.File_Descriptor;
      flags   : T_open_flags) return Pkg_Error_Type
   is
      arc     : libarchive_h.archive_Access;
      arcent  : libarchive_h.archive_entry_Access;
      retcode : Pkg_Error_Type;
   begin

      retcode := pkg_open2 (pkg_ptr => pkg_ptr,
                            arc     => arc,
                            arcent  => arcent,
                            path    => "",
                            flags   => flags,
                            fd      => fd);

      if retcode /= EPKG_OK and then
        retcode /= EPKG_END
      then
         return EPKG_FATAL;
      end if;

      return EPKG_OK;
   end pkg_open_fd;


   --------------------------------------------------------------------
   --  pkg_open2
   --------------------------------------------------------------------
   function pkg_open2
     (pkg_ptr : in out T_pkg_Access;
      arc     : in out libarchive_h.archive_Access;
      arcent  : in out libarchive_h.archive_entry_Access;
      path    : String;
      flags   : T_open_flags;
      fd      : Unix.File_Descriptor) return Pkg_Error_Type
   is
      procedure cleanup (arc_only : Boolean := False);

      block_size     : constant := 4096;
      manifest_found : Boolean := False;
      data_final     : Boolean;
      data_error     : Boolean;

      procedure cleanup (arc_only : Boolean := False)
      is
         --  only called when result not EPKG_OK or EPKG_END
      begin
         libarchive.read_close (arc);
         libarchive.read_free (arc);
         arc     := null;
         arcent  := null;

         if not arc_only then
            delete_pkg (pkg_ptr);
            pkg_ptr := null;
         end if;
      end cleanup;
   begin
      arc := libarchive_h.archive_read_new;
      libarchive.read_support_filter_all (arc);
      libarchive.read_support_format_tar (arc);

      if Unix.file_connected (fd) then
         begin
            libarchive.read_open_fd (arc, fd, block_size);
         exception
            when libarchive.archive_error =>
               if (flags and PKG_OPEN_TRY) = 0 then
                  Event.pkg_emit_error (SUS ("archive_read_open_filename (" & path & "): " &
                                          libarchive.error_string (arc)));
               end if;
               cleanup;
               return EPKG_FATAL;
         end;
      else
         begin
            libarchive.read_open_filename (arc, path, block_size);
         exception
            when libarchive.archive_error =>
               if (flags and PKG_OPEN_TRY) = 0 then
                  Event.pkg_emit_error (SUS ("archive_read_open_fd: " &
                                          libarchive.error_string (arc)));
               end if;
               cleanup;
               return EPKG_FATAL;
         end;
      end if;

      pkg_ptr := new T_pkg;

      loop
         exit when not libarchive.read_next_header (arc, arcent, data_final, data_error);

         declare
            fpath   : constant String := libarchive.entry_pathname (arcent);
            len     : libarchive.arc64;
            retcode : Pkg_Error_Type;
         begin
            if fpath'Length > 0 and then fpath (fpath'First) = '+' then
               if fpath = "+COMPACT_MANIFEST" then
                  len := libarchive.entry_size (arcent);
                  begin
                     declare
                        data : String := libarchive.read_data (arc, len);
                     begin
                        retcode := Manifest.pkg_parse_manifest (pkg_ptr, data);
                        if retcode /= EPKG_OK then
                           cleanup;
                           return EPKG_FATAL;
                        end if;
                        manifest_found := True;
                        exit;
                     end;
                  exception
                     when libarchive.archive_error =>
                        cleanup;
                        return EPKG_FATAL;
                  end;
               elsif fpath = "+MANIFEST" then
                  len := libarchive.entry_size (arcent);
                  begin
                     declare
                        data : String := libarchive.read_data (arc, len);
                     begin
                        retcode := Manifest.pkg_parse_manifest (pkg_ptr, data);
                        if retcode /= EPKG_OK then
                           if (flags and PKG_OPEN_TRY) = 0 then
                              Event.pkg_emit_error
                                (SUS (path & " is not a valid package: invalid manifest"));
                           end if;
                           cleanup;
                           return EPKG_FATAL;
                        end if;
                        manifest_found := True;
                        if (flags and PKG_OPEN_MANIFEST_ONLY) > 0 then
                           exit;
                        end if;
                     end;
                  exception
                     when libarchive.archive_error =>
                        cleanup;
                        return EPKG_FATAL;
                  end;
               end if;
            end if;
         end;
      end loop;

      if data_error then
         if (flags and PKG_OPEN_TRY) = 0 then
            Event.pkg_emit_error
              (SUS ("archive_read_next_header(): " & libarchive.error_string (arc)));
         end if;
         cleanup;
         return EPKG_FATAL;
      end if;

      if not manifest_found then
         if (flags and PKG_OPEN_TRY) = 0 then
            Event.pkg_emit_error (SUS (path & " is not a valid package: no manifest found"));
         end if;
         cleanup;
         return EPKG_FATAL;
      end if;

      cleanup (arc_only => True);

      if data_final then
         return EPKG_END;
      else
         return EPKG_OK;
      end if;

   end pkg_open2;

end Core.Pkg;
