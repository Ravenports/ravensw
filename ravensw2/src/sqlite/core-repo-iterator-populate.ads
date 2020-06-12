--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Pkgtypes;
with SQLite;
with sqlite_h;

package Core.Repo.Iterator.Populate is

   procedure populate_pkg
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access);

   --  Iterate section type, load all sections by default.
   --  If "sections" argument is passed, selectively load sections
   function ensure_sections_loaded
     (db         : sqlite_h.sqlite3_Access;
      pkg_access : Pkgtypes.A_Package_Access;
      sections   : Pkgtypes.Package_Load_Flags := (others => True)) return Action_Result;

private

   internal_srcfile : constant String := "core-repo-iterator-populate.adb";

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
      PKG_VITAL,
      PKG_NUM_FIELDS);

   function get_attribute (column_name : String) return pkg_attr;

   function add_category
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_license
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_user
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_group
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_shlib_prov
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_shlib_reqd
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_provides
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_requires
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_conflict
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_annotation
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_directory
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_scripts
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_rdeps
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_files
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_config_files
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_options
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function add_deps
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   function no_operation
     (stmt       : SQLite.thick_stmt;
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

   type val_load_callback is access function
     (stmt       : SQLite.thick_stmt;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result;

   load_val_operation : constant array (Pkgtypes.Load_Section'Range) of val_load_callback :=
     (Pkgtypes.basic           => no_operation'Access,
      Pkgtypes.deps            => add_deps'Access,
      Pkgtypes.rdeps           => add_rdeps'Access,
      Pkgtypes.files           => add_files'Access,
      Pkgtypes.scripts         => add_scripts'Access,
      Pkgtypes.options         => add_options'Access,
      Pkgtypes.dirs            => add_directory'Access,
      Pkgtypes.categories      => add_category'Access,
      Pkgtypes.licenses        => add_license'Access,
      Pkgtypes.users           => add_user'Access,
      Pkgtypes.groups          => add_group'Access,
      Pkgtypes.shlibs_requires => add_shlib_reqd'Access,
      Pkgtypes.shlibs_provided => add_shlib_prov'Access,
      Pkgtypes.annotations     => add_annotation'Access,
      Pkgtypes.conflicts       => add_conflict'Access,
      Pkgtypes.provides        => add_provides'Access,
      Pkgtypes.requires        => add_requires'Access,
      Pkgtypes.config_files    => add_config_files'Access);

   procedure clear_section (pkg_access : Pkgtypes.A_Package_Access;
                            section    : Pkgtypes.Load_Section);

   --  Called by each individual section to load query results into package
   function load_val
     (db         : sqlite_h.sqlite3_Access;
      pkg_access : Pkgtypes.A_Package_Access;
      section    : Pkgtypes.Load_Section;
      sql        : String) return Action_Result;

   function get_section_sql (section : Pkgtypes.Load_Section) return String;



end Core.Repo.Iterator.Populate;
