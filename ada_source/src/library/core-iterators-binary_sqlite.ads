--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with sqlite_h;

package Core.Iterators.Binary_sqlite is

   type Iterator_Binary_Sqlite is new Base_Iterators with private;

   overriding
   procedure Free  (this : in out Iterator_Binary_Sqlite);

   overriding
   procedure Reset (this : in out Iterator_Binary_Sqlite);

   overriding
   function Next (this    : in out Iterator_Binary_Sqlite;
                  pkg_ptr : T_pkg_Access;
                  flags   : Load_Flags) return Pkg_Error_Type;

   overriding
   function create_invalid_iterator return Iterator_Binary_Sqlite;

   function create
     (db           : sqlite_h.sqlite3_Access;
      stmt         : sqlite_h.sqlite3_stmt_Access;
      package_type : pkg_type;
      flags        : Iterator_Flags) return Iterator_Binary_Sqlite;

private

   type Iterator_Binary_Sqlite is new Base_Iterators with
      record
         db           : sqlite_h.sqlite3_Access;
         stmt         : sqlite_h.sqlite3_stmt_Access;
         counter      : Natural;
         package_type : pkg_type;
         flags        : Iterator_Flags;
      end record;

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

   type PKG_LOAD_OPS is
     (PKG_LOAD_DEPS,
      PKG_LOAD_RDEPS,
      PKG_LOAD_FILES,
      PKG_LOAD_CONFIG_FILES,
      PKG_LOAD_DIRS,
      PKG_LOAD_SCRIPTS,
      PKG_LOAD_OPTIONS,
      PKG_LOAD_CATEGORIES,
      PKG_LOAD_LICENSES,
      PKG_LOAD_USERS,
      PKG_LOAD_GROUPS,
      PKG_LOAD_SHLIBS_REQUIRED,
      PKG_LOAD_SHLIBS_PROVIDED,
      PKG_LOAD_ANNOTATIONS,
      PKG_LOAD_CONFLICTS,
      PKG_LOAD_PROVIDES,
      PKG_LOAD_REQUIRES,
      PKG_LOAD_DEP_FORMULA);

   function get_attribute (column_name : String) return pkg_attr;

   procedure populate_pkg (stmt : sqlite_h.sqlite3_stmt_Access;
                           pkg_access : out T_pkg_Access);

   type pkg_load_callback is access function
     (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access) return Pkg_Error_Type;

   type val_load_callback is access function
     (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access) return Pkg_Error_Type;

   function pkgdb_load_deps (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                             return Pkg_Error_Type;
   function pkgdb_load_rdeps (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                              return Pkg_Error_Type;
   function pkgdb_load_files (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                              return Pkg_Error_Type;
   function pkgdb_load_dirs (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                             return Pkg_Error_Type;
   function pkgdb_load_scripts (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                return Pkg_Error_Type;
   function pkgdb_load_options (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                return Pkg_Error_Type;
   function pkgdb_load_categories (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type;
   function pkgdb_load_licenses (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type;
   function pkgdb_load_users (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                              return Pkg_Error_Type;
   function pkgdb_load_groups (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                               return Pkg_Error_Type;
   function pkgdb_load_shlib_reqd (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                   return Pkg_Error_Type;
   function pkgdb_load_shlib_prov (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                   return Pkg_Error_Type;
   function pkgdb_load_annotations (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                    return Pkg_Error_Type;
   function pkgdb_load_conflicts (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                  return Pkg_Error_Type;
   function pkgdb_load_provides (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type;
   function pkgdb_load_requires (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type;
   function pkgdb_load_config_file (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                    return Pkg_Error_Type;
   function pkgdb_load_dep_formula (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                    return Pkg_Error_Type;

   type load_on_flag_record is
      record
         flag : Load_Flags;
         load : pkg_load_callback;
      end record;

   load_on_flag : constant array (PKG_LOAD_OPS'Range) of load_on_flag_record :=
     (PKG_LOAD_DEPS            => (PKG_LOAD_FLAG_DEPS, pkgdb_load_deps'Access),
      PKG_LOAD_DEP_FORMULA     => (PKG_LOAD_FLAG_DEP_FORMULA, pkgdb_load_dep_formula'Access),
      PKG_LOAD_RDEPS           => (PKG_LOAD_FLAG_RDEPS, pkgdb_load_rdeps'Access),
      PKG_LOAD_FILES           => (PKG_LOAD_FLAG_FILES, pkgdb_load_files'Access),
      PKG_LOAD_CONFIG_FILES    => (PKG_LOAD_FLAG_CONFIG_FILES, pkgdb_load_config_file'Access),
      PKG_LOAD_DIRS            => (PKG_LOAD_FLAG_DIRS, pkgdb_load_dirs'Access),
      PKG_LOAD_SCRIPTS         => (PKG_LOAD_FLAG_SCRIPTS, pkgdb_load_scripts'Access),
      PKG_LOAD_OPTIONS         => (PKG_LOAD_FLAG_OPTIONS, pkgdb_load_options'Access),
      PKG_LOAD_CATEGORIES      => (PKG_LOAD_FLAG_CATEGORIES, pkgdb_load_categories'Access),
      PKG_LOAD_LICENSES        => (PKG_LOAD_FLAG_LICENSES, pkgdb_load_licenses'Access),
      PKG_LOAD_USERS           => (PKG_LOAD_FLAG_USERS, pkgdb_load_users'Access),
      PKG_LOAD_GROUPS          => (PKG_LOAD_FLAG_GROUPS, pkgdb_load_groups'Access),
      PKG_LOAD_REQUIRES        => (PKG_LOAD_FLAG_REQUIRES, pkgdb_load_requires'Access),
      PKG_LOAD_PROVIDES        => (PKG_LOAD_FLAG_PROVIDES, pkgdb_load_provides'Access),
      PKG_LOAD_ANNOTATIONS     => (PKG_LOAD_FLAG_ANNOTATIONS, pkgdb_load_annotations'Access),
      PKG_LOAD_SHLIBS_REQUIRED => (PKG_LOAD_FLAG_SHLIBS_REQUIRED, pkgdb_load_shlib_reqd'Access),
      PKG_LOAD_SHLIBS_PROVIDED => (PKG_LOAD_FLAG_SHLIBS_PROVIDED, pkgdb_load_shlib_prov'Access),
      PKG_LOAD_CONFLICTS       => (PKG_LOAD_FLAG_CONFLICTS, pkgdb_load_conflicts'Access)
     );

   function add_category (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                          return Pkg_Error_Type;
   function add_license (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                         return Pkg_Error_Type;
   function add_user (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                      return Pkg_Error_Type;
   function add_group (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                       return Pkg_Error_Type;
   function add_shlib_prov (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                            return Pkg_Error_Type;
   function add_shlib_reqd (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                            return Pkg_Error_Type;
   function add_provides (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                          return Pkg_Error_Type;
   function add_requires (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                          return Pkg_Error_Type;
   function add_conflict (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                          return Pkg_Error_Type;
   function add_annotation (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                            return Pkg_Error_Type;
   function add_directory (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                           return Pkg_Error_Type;
   function add_scripts (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                         return Pkg_Error_Type;
   function add_rdeps (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                       return Pkg_Error_Type;
   function add_files (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                       return Pkg_Error_Type;
   function add_config_files (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                              return Pkg_Error_Type;
   function add_options (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                         return Pkg_Error_Type;
   function add_deps (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                      return Pkg_Error_Type;

   load_val_operation : constant array (PKG_LOAD_OPS'Range) of val_load_callback :=
     (PKG_LOAD_DEPS            => add_deps'Access,
      PKG_LOAD_DEP_FORMULA     => null,
      PKG_LOAD_RDEPS           => add_rdeps'Access,
      PKG_LOAD_FILES           => add_files'Access,
      PKG_LOAD_CONFIG_FILES    => add_config_files'Access,
      PKG_LOAD_DIRS            => add_directory'Access,
      PKG_LOAD_SCRIPTS         => add_scripts'Access,
      PKG_LOAD_OPTIONS         => add_options'Access,
      PKG_LOAD_CATEGORIES      => add_category'Access,
      PKG_LOAD_LICENSES        => add_license'Access,
      PKG_LOAD_USERS           => add_user'Access,
      PKG_LOAD_GROUPS          => add_group'Access,
      PKG_LOAD_REQUIRES        => add_requires'Access,
      PKG_LOAD_PROVIDES        => add_provides'Access,
      PKG_LOAD_ANNOTATIONS     => add_annotation'Access,
      PKG_LOAD_SHLIBS_REQUIRED => add_shlib_reqd'Access,
      PKG_LOAD_SHLIBS_PROVIDED => add_shlib_prov'Access,
      PKG_LOAD_CONFLICTS       => add_conflict'Access
     );

   function load_val
     (db         : sqlite_h.sqlite3_Access;
      pkg_access : T_pkg_Access;
      operation  : PKG_LOAD_OPS;
      sql        : String)
      return Pkg_Error_Type;


end Core.Iterators.Binary_sqlite;
