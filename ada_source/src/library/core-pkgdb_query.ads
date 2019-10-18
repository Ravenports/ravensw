--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Iterators.Binary_sqlite;
with Core.PkgDB;

package Core.PkgDB_Query is

   package IBS renames Core.Iterators.Binary_sqlite;

   function pkgdb_get_pattern_query (pattern : String; match : PkgDB.T_match) return String;

   function pkgdb_query
     (db      : PkgDB.struct_pkgdb;
      pattern : String;
      match   : PkgDB.T_match) return IBS.Iterator_Binary_Sqlite;

end Core.PkgDB_Query;
