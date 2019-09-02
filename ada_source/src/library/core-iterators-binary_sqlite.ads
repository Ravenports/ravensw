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
   function Next (this : in out Iterator_Binary_Sqlite;
                  pkg_ptr : access T_pkg_Access;
                  flags : Iterator_Flags) return Pkg_Error_Type;
private

   type Iterator_Binary_Sqlite is new Base_Iterators with
      record
         db            : sqlite_h.sqlite3_Access;
         stmt          : sqlite_h.sqlite3_stmt_Access;
         counter       : Natural;
         package_type  : pkg_type;
         flags         : Iterator_Flags;
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

   function get_attribute (column_name : String) return pkg_attr;

   procedure populate_pkg (stmt : sqlite_h.sqlite3_stmt_Access;
                           pkg_access : T_pkg_Access);

   procedure clear_pkg_data (pkg_access : T_pkg_Access);

end Core.Iterators.Binary_sqlite;
