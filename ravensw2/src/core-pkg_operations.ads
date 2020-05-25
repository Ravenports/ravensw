--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkgtypes;

package Core.Pkg_Operations is

   function pkg_adddir_attr
     (pkg_access : Pkgtypes.A_Package_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : Pkgtypes.mode_t;
      fflags : Pkgtypes.Package_Dir_Flags;
      check_duplicates : Boolean) return Action_Result;

   function pkg_addrdep
     (pkg_access : Pkgtypes.A_Package_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Action_Result;

   function pkg_addconfig_file
     (pkg_access : Pkgtypes.A_Package_Access;
      path       : Text;
      content    : Text) return Action_Result;

   function pkg_addfile_attr
     (pkg_access : Pkgtypes.A_Package_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : Pkgtypes.mode_t;
      fflags : Pkgtypes.Package_Dir_Flags;
      sum    : Text;
      check_duplicates : Boolean) return Action_Result;

   function pkg_addoption
     (pkg_access : Pkgtypes.A_Package_Access;
      key        : Text;
      value      : Text) return Action_Result;

   function pkg_adddep
     (pkg_access : Pkgtypes.A_Package_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Action_Result;

   function pkg_adddir
     (pkg_access : Pkgtypes.A_Package_Access;
      path       : Text;
      check_duplicates : Boolean) return Action_Result;

   function pkg_addfile
     (pkg_access : Pkgtypes.A_Package_Access;
      path       : Text;
      sum        : Text;
      check_duplicates : Boolean) return Action_Result;

   function pkg_addscript
     (pkg_access : Pkgtypes.A_Package_Access;
      data       : Text;
      script     : Pkgtypes.Package_Script) return Action_Result;

   function pkg_addshlib_required
     (pkg_access : Pkgtypes.A_Package_Access;
      data       : String) return Action_Result;

   function pkg_addshlib_provided
     (pkg_access : Pkgtypes.A_Package_Access;
      data       : String) return Action_Result;

   function pkg_addconflict
     (pkg_access : Pkgtypes.A_Package_Access;
      data       : String) return Action_Result;

   function pkg_addstring
     (crate      : in out Pkgtypes.Text_Crate.Vector;
      data       : String;
      title      : String) return Action_Result;

   function pkg_addstring_silent_unique
     (crate      : in out Pkgtypes.Text_Crate.Vector;
      data       : String) return Action_Result;


end Core.Pkg_Operations;
