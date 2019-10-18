--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings; use Core.Strings;
with Core.Pkg;     use Core.Pkg;
with Core.Event;
with Core.Iterators;

with SQLite;
with sqlite_h;

package body Core.PkgDB_Query is

   --------------------------------------------------------------------
   --  pkgdb_get_pattern_query
   --------------------------------------------------------------------
   function pkgdb_get_pattern_query (pattern : String; match : PkgDB.T_match) return String
   is
      checkorigin : Text;
      checkuid    : Text;
      tilde       : constant String := "~";
   begin
      if contains (pattern, tilde) then
         checkuid := SUS (tilde & part_2 (pattern, tilde));
      else
         checkorigin := SUS ("/" & part_2 (pattern, "/"));
      end if;

      case match is

         when PkgDB.MATCH_ALL =>
            return "";

         when PkgDB.MATCH_CONDITION =>
            return pattern;

         when PkgDB.MATCH_REGEX =>
            if IsBlank (checkuid) then
               if IsBlank (checkorigin) then
                  return " WHERE name REGEXP ?1 OR name || '-' || version REGEXP ?1";
               else
                  return " WHERE origin REGEXP ?1";
               end if;
            else
               return " WHERE name = ?1";
            end if;

         when PkgDB.MATCH_GLOB =>
            if IsBlank (checkuid) then
               if IsBlank (checkorigin) then
                  return " WHERE name GLOB ?1 OR name || '-' || version GLOB ?1";
               else
                  return " WHERE origin GLOB ?1";
               end if;
            else
               return " WHERE name = ?1";
            end if;

         when PkgDB.MATCH_EXACT =>
            if PkgDB.pkgdb_is_case_sensitive then
               if IsBlank (checkuid) then
                  if IsBlank (checkorigin) then
                     return " WHERE name = ?1 " &
                       "OR (name = SPLIT_VERSION('name', ?1) AND " &
                       " version = SPLIT_VERSION('version', ?1))";
                  else
                     return " WHERE origin = ?1";
                  end if;
               else
                  return " WHERE name = ?1";
               end if;
            else
               if IsBlank (checkuid) then
                  if IsBlank (checkorigin) then
                     return " WHERE name = ?1 COLLATE NOCASE " &
                       "OR (name = SPLIT_VERSION('name', ?1) COLLATE NOCASE AND " &
                       " version = SPLIT_VERSION('version', ?1))";
                  else
                     return " WHERE origin = ?1 COLLATE NOCASE";
                  end if;
               else
                  return " WHERE name = ?1 COLLATE NOCASE";
               end if;
            end if;

      end case;
   end pkgdb_get_pattern_query;


   --------------------------------------------------------------------
   --  pkgdb_query
   --------------------------------------------------------------------
   function pkgdb_query
     (db      : PkgDB.struct_pkgdb;
      pattern : String;
      match   : PkgDB.T_match) return IBS.Iterator_Binary_Sqlite
   is
      use type PkgDB.T_match;
   begin
      if match /= PkgDB.MATCH_ALL and then IsBlank (pattern) then
         return IBS.create_invalid_iterator;
      end if;

      declare
         stmt : aliased sqlite_h.sqlite3_stmt_Access;
         comp : constant String := pkgdb_get_pattern_query (pattern, match);
         dbs  : sqlite_h.sqlite3_Access := PkgDB.get_sqlite_access (db);
         sql  : constant String :=
           "SELECT id, origin, name, name as uniqueid, version, comment, desc," &
           "  message, arch, maintainer, www, prefix, flatsize, licenselogic, automatic," &
           "  locked, time, manifestdigest, vital " &
           "FROM packages AS p" &
           comp & " " &
           "ORDER BY p.name;";
      begin
         Event.pkg_debug (4, "Pkgdb: running '" & sql & "'");
         if SQLite.prepare_sql (pDB    => dbs,
                                sql    => sql,
                                ppStmt => stmt'Access)
         then
            if match /= PkgDB.MATCH_ALL and then
              match /= PkgDB.MATCH_CONDITION
            then
               SQLite.bind_string (stmt, 1, pattern);
            end if;
            return IBS.create (db           => dbs,
                               stmt         => stmt,
                               package_type => PKG_INSTALLED,
                               flags        => Iterators.PKGDB_IT_FLAG_ONCE);
         else
            PkgDB.ERROR_SQLITE (dbs, "pkgdb_query (prep)", sql);
            return IBS.create_invalid_iterator;
         end if;
      end;
   end pkgdb_query;

end Core.PkgDB_Query;
