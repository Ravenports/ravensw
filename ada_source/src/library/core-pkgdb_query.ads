--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.PkgDB;

package Core.pkgdb_query is

   function pkgdb_get_pattern_query (pattern : String; match : PkgDB.T_match) return String;

end Core.pkgdb_query;
