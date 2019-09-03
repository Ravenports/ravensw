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

end Core.Pkg;
