--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Event;
with Core.Strings;
with Core.Utilities;
with Core.Context;

use Core.Strings;

package body Core.Pkg_Operations is

   --------------------------------------------------------------------
   --  pkg_adddir_attr
   --------------------------------------------------------------------
   function pkg_adddir_attr
     (pkg_access : Pkgtypes.A_Package_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : Pkgtypes.mode_t;
      fflags : Pkgtypes.Package_Dir_Flags;
      check_duplicates : Boolean) return Action_Result
   is
      D : Pkgtypes.Package_Directory;
      fullpath : Text;
   begin
      if not contains (path, "/") then
         Event.emit_error ("skipping useless directory: " & SQ (USS (path)));
      end if;

      fullpath := SUS (Utilities.pkg_absolutepath (USS (path), False));
      Event.emit_debug (3, "Pkg: add new directory " & SQ (USS (fullpath)));
      if check_duplicates and then
        pkg_access.dirs.Contains (fullpath)
      then
         if Context.reveal_developer_mode then
            Event.emit_error
              ("duplicate directory listing: " & USS (fullpath) & ", fatal (developer mode)");
            return (RESULT_FATAL);
         else
            Event.emit_error ("duplicate directory listing: " & USS (fullpath) & ", ignoring");
         end if;
      end if;

      D.path   := fullpath;
      D.uname  := uname;
      D.gname  := gname;
      D.perm   := perm;
      D.fflags := fflags;

      pkg_access.dirs.Insert (fullpath, D);
      return RESULT_OK;
   end pkg_adddir_attr;


   --------------------------------------------------------------------
   --  pkg_addrdep
   --------------------------------------------------------------------
   function pkg_addrdep
     (pkg_access : Pkgtypes.A_Package_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Action_Result
   is
      D : Pkgtypes.Package_Dependency;
   begin
      Event.emit_debug
        (3, "Pkg: add a new reverse dependency origin: " & USS (origin) & ", name: " & USS (name));

      D.name    := name;
      D.origin  := origin;
      D.version := version;
      D.locked  := locked;
      D.uid     := name;

      pkg_access.rdepends.Prepend (D);
      return RESULT_OK;
   end pkg_addrdep;


   --------------------------------------------------------------------
   --  pkg_addconfig_file
   --------------------------------------------------------------------
   function pkg_addconfig_file
     (pkg_access : Pkgtypes.A_Package_Access;
      path       : Text;
      content    : Text) return Action_Result
   is
      CF : Pkgtypes.Package_Config_File;
      fullpath : Text;
   begin
      fullpath := SUS (Utilities.pkg_absolutepath (USS (path), False));
      Event.emit_debug (3, "Pkg: add new config file " & SQ (USS (fullpath)));
      if pkg_access.config_files.Contains (fullpath) then
         if Context.reveal_developer_mode then
            Event.emit_error
              ("duplicate config file listing: " & USS (fullpath) & ", fatal (developer mode)");
            return (RESULT_FATAL);
         else
            Event.emit_error ("duplicate config file listing: " & USS (fullpath) & ", ignoring");
         end if;
      end if;

      CF.path := fullpath;
      CF.content := content;

      pkg_access.config_files.Insert (fullpath, CF);
      return RESULT_OK;
   end pkg_addconfig_file;


   --------------------------------------------------------------------
   --  pkg_addfile_attr
   --------------------------------------------------------------------
   function pkg_addfile_attr
     (pkg_access : Pkgtypes.A_Package_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : Pkgtypes.mode_t;
      fflags : Pkgtypes.Package_Dir_Flags;
      sum    : Text;
      check_duplicates : Boolean) return Action_Result
   is
      F : Pkgtypes.Package_File;
      fullpath : Text;
   begin
      fullpath := SUS (Core.Utilities.pkg_absolutepath (USS (path), False));
      Event.emit_debug (3, "Pkg: add new file " & SQ (USS (fullpath)));
      if check_duplicates and then
        pkg_access.dirs.Contains (fullpath)
      then
         if Context.reveal_developer_mode then
            Event.emit_error
              ("duplicate file listing: " & USS (fullpath) & ", fatal (developer mode)");
            return (RESULT_FATAL);
         else
            Event.emit_error ("duplicate file listing: " & USS (fullpath) & ", ignoring");
         end if;
      end if;

      F.path   := fullpath;
      F.uname  := uname;
      F.gname  := gname;
      F.perm   := perm;
      F.fflags := fflags;
      F.sum    := sum;

      pkg_access.files.Append (F);
      return RESULT_OK;
   end pkg_addfile_attr;


   --------------------------------------------------------------------
   --  pkg_addoption
   --------------------------------------------------------------------
   function pkg_addoption
     (pkg_access : Pkgtypes.A_Package_Access;
      key        : Text;
      value      : Text) return Action_Result is
   begin
      Event.emit_debug (2, "Pkg: adding options: " & USS (key) & " = " & USS (value));
      if pkg_access.options.Contains (key) then
         if Context.reveal_developer_mode then
            Event.emit_error
              ("duplicate option listing: " & USS (key) & ", fatal (developer mode)");
            return (RESULT_FATAL);
         else
            Event.emit_error ("duplicate option listing: " & USS (key) & ", ignoring");
         end if;
      end if;

      pkg_access.options.Insert (key, value);
      return RESULT_OK;
   end pkg_addoption;


   --------------------------------------------------------------------
   --  pkg_adddep
   --------------------------------------------------------------------
   function pkg_adddep
     (pkg_access : Pkgtypes.A_Package_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Action_Result
   is
      D : Pkgtypes.Package_Dependency;
   begin
      Event.emit_debug
        (3, "Pkg: add a new dependency origin: " & USS (origin) & ", name: " & USS (name));

      if pkg_access.depends.Contains (name) then
         Event.emit_error
           (USS (pkg_access.name) & ": duplicate dependency listing: " & USS (name));
         return RESULT_FATAL;
      end if;

      D.name    := name;
      D.origin  := origin;
      D.version := version;
      D.locked  := locked;
      D.uid     := name;

      pkg_access.depends.Insert (name, D);
      return RESULT_OK;
   end pkg_adddep;


   --------------------------------------------------------------------
   --  pkg_adddir
   --------------------------------------------------------------------
   function pkg_adddir
     (pkg_access : Pkgtypes.A_Package_Access;
      path       : Text;
      check_duplicates : Boolean) return Action_Result is
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
     (pkg_access : Pkgtypes.A_Package_Access;
      path       : Text;
      sum        : Text;
      check_duplicates : Boolean) return Action_Result
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
     (pkg_access : Pkgtypes.A_Package_Access;
      data       : Text;
      script     : Pkgtypes.Package_Script) return Action_Result is
   begin
      pkg_access.scripts (script) := data;
      return RESULT_OK;
   end pkg_addscript;


   --------------------------------------------------------------------
   --  pkg_addshlib_required
   --------------------------------------------------------------------
   function pkg_addshlib_required
     (pkg_access : Pkgtypes.A_Package_Access;
      data       : String) return Action_Result
   is
      datatxt : Text := SUS (data);
   begin
      --  silently ignore duplicates in case of shlibs
      if not pkg_access.shlibs_reqd.Contains (datatxt) then
         pkg_access.shlibs_reqd.Append (datatxt);
         Event.emit_debug (3, "added shlib deps for " & USS (pkg_access.name) & " on " & data);
      end if;
      return RESULT_OK;
   end pkg_addshlib_required;


   --------------------------------------------------------------------
   --  pkg_addshlib_provided
   --------------------------------------------------------------------
   function pkg_addshlib_provided
     (pkg_access : Pkgtypes.A_Package_Access;
      data       : String) return Action_Result
   is
      datatxt : Text := SUS (data);
   begin
      --  silently ignore files which are not starting with lib
      if leads (data, "lib") then
         --  silently ignore duplicates in case of shlibs
         if not pkg_access.shlibs_prov.Contains (datatxt) then
            pkg_access.shlibs_prov.Append (datatxt);
            Event.emit_debug
              (3, "added shlib provided for " & USS (pkg_access.name) & " on " & data);
         end if;
      end if;
      return RESULT_OK;
   end pkg_addshlib_provided;


   --------------------------------------------------------------------
   --  pkg_addconflict
   --------------------------------------------------------------------
   function pkg_addconflict
     (pkg_access : Pkgtypes.A_Package_Access;
      data       : String) return Action_Result
   is
      unique_id : Text := SUS (data);
      conflict  : Pkgtypes.Package_Conflict;
   begin
      --  silently ignore duplicates in case of conflicts
      if not pkg_access.conflicts.Contains (unique_id) then
         conflict.uid := unique_id;  --  digest and type not set (should it be?)
         pkg_access.conflicts.Insert (unique_id, conflict);
         Event.emit_debug
           (3, "add a new conflict origin: " & USS (pkg_access.uid) & ", with " & data);
      end if;
      return RESULT_OK;
   end pkg_addconflict;


   --------------------------------------------------------------------
   --  pkg_addstring
   --------------------------------------------------------------------
   function pkg_addstring
     (crate      : in out Pkgtypes.Text_Crate.Vector;
      data       : String;
      title      : String) return Action_Result
   is
      datatxt : Text := SUS (data);
   begin
      if crate.Contains (datatxt) then
         if Context.reveal_developer_mode then
            Event.emit_error
              ("duplicate " & title & " listing: " & data & ", fatal (developer mode)");
            return RESULT_FATAL;
         else
            Event.emit_error ("duplicate " & title & " listing: " & data & ", ignoring");
         end if;
      else
         crate.Append (datatxt);
      end if;
      return RESULT_OK;
   end pkg_addstring;


   --------------------------------------------------------------------
   --  pkg_addstring_silent_unique
   --------------------------------------------------------------------
   function pkg_addstring_silent_unique
     (crate      : in out Pkgtypes.Text_Crate.Vector;
      data       : String) return Action_Result
   is
      datatxt : Text := SUS (data);
   begin
      if not crate.Contains (datatxt) then
         crate.Append (datatxt);
      end if;
      return RESULT_OK;
   end pkg_addstring_silent_unique;


end Core.Pkg_Operations;
