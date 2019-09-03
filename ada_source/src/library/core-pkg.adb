--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Event;
with Core.Utilities;
with Core.Strings;

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

end Core.Pkg;
