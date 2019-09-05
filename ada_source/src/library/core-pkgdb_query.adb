--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings; use Core.Strings;

package body Core.pkgdb_query is

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

end Core.pkgdb_query;
