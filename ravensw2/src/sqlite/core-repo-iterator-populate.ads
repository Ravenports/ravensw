--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Pkgtypes;
with sqlite_h;

package Core.Repo.Iterator.Populate is

   procedure populate_pkg (stmt : sqlite_h.sqlite3_stmt_Access;
                           pkg_access : Pkgtypes.A_Package_Access);

private

   type pkg_attr is
     (NOTFOUND,
      PKG_ORIGIN,
      PKG_NAME,
      PKG_VERSION,
      PKG_COMMENT,
      PKG_DESC,
      PKG_MTREE,
      PKG_MESSAGE,
      PKG_ARCH,
      PKG_ABI,
      PKG_MAINTAINER,
      PKG_WWW,
      PKG_PREFIX,
      PKG_REPOPATH,
      PKG_CKSUM,
      PKG_OLD_VERSION,
      PKG_REPONAME,
      PKG_REPOURL,
      PKG_DIGEST,
      PKG_REASON,
      PKG_FLATSIZE,
      PKG_OLD_FLATSIZE,
      PKG_PKGSIZE,
      PKG_LICENSE_LOGIC,
      PKG_AUTOMATIC,
      PKG_LOCKED,
      PKG_ROWID,
      PKG_TIME,
      PKG_ANNOTATIONS,
      PKG_UNIQUEID,
      PKG_OLD_DIGEST,
      PKG_DEP_FORMULA,
      PKG_VITAL,
      PKG_NUM_FIELDS);

   function get_attribute (column_name : String) return pkg_attr;

   function add_category
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_license
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_user
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_group
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_shlib_prov
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_shlib_reqd
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_provides
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_requires
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_conflict
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_annotation
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_directory
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_scripts
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_rdeps
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_files
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_config_files
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_options
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_deps
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function pkg_adddir_attr
     (pkg_access : Pkgtypes.A_Package_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : Pkgtypes.mode_t;
      fflags : Pkgtypes.Package_Dir_Flags;
      check_duplicates : Boolean) return Action_Result;

   function pkg_addfile_attr
     (pkg_access : Pkgtypes.A_Package_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : Pkgtypes.mode_t;
      fflags : Pkgtypes.Package_Dir_Flags;
      sum    : Text;
      check_duplicates : Boolean) return Action_Result;

   function pkg_adddep
     (pkg_access : Pkgtypes.A_Package_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Action_Result;

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

   function pkg_addoption
     (pkg_access : Pkgtypes.A_Package_Access;
      key        : Text;
      value      : Text) return Action_Result;

end Core.Repo.Iterator.Populate;
