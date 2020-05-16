--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;

package body Core.Database is

   --------------------------------------------------------------------
   --  set_case_sensitivity
   --------------------------------------------------------------------
   procedure set_case_sensitivity (sensitive : Boolean) is
   begin
      case_sensitivity_setting := sensitive;
   end set_case_sensitivity;


   --------------------------------------------------------------------
   --  case_sensitivity_is_on
   --------------------------------------------------------------------
   function case_sensitivity_is_on return Boolean is
   begin
      return case_sensitivity_setting;
   end case_sensitivity_is_on;


   --------------------------------------------------------------------
   --  set_match_behavior
   --------------------------------------------------------------------
   function set_match_behavior
     (request_exact : Boolean := False;
      request_glob  : Boolean := False;
      request_regex : Boolean := False;
      request_condition : Boolean := False) return Match_Behavior is
   begin
      if request_exact then
         return MATCH_EXACT;
      elsif request_glob then
         return MATCH_GLOB;
      elsif request_regex then
         return MATCH_REGEX;
      elsif request_condition then
         return MATCH_CONDITION;
      else
         return MATCH_ALL;
      end if;
   end set_match_behavior;


   --------------------------------------------------------------------
   --  get_pattern_query
   --------------------------------------------------------------------
   function get_pattern_query (pattern : String; match_style : Match_Behavior) return String is
   begin
      case match_style is
         when MATCH_ALL => return "";
         when others => null;
      end case;

      declare
         checkorigin : Text;
         checkuid    : Text;

         tilda : constant String := "~";
         slash : constant String := "/";
      begin

         if not Strings.IsBlank (pattern) then
            if Strings.contains (pattern, tilda) then
               checkuid := Strings.SUS (tilda & Strings.part_2 (pattern, tilda));
            elsif Strings.contains (pattern, slash) then
               checkorigin := Strings.SUS (slash & Strings.part_2 (pattern, slash));
            end if;
         end if;

         case match_style is
            when MATCH_ALL =>
               return "";  --  Will never reach here
            when MATCH_CONDITION =>
               return pattern;
            when MATCH_GLOB =>
               if Strings.IsBlank (checkuid) then
                  if Strings.IsBlank (checkorigin) then
                     return " WHERE name GLOB ?1 OR name || '-' || version GLOB ?1";
                  else
                     return " WHERE origin GLOB ?1";
                  end if;
               else
                  return " WHERE name = ?1";
               end if;
            when MATCH_REGEX =>
               if Strings.IsBlank (checkuid) then
                  if Strings.IsBlank (checkorigin) then
                     return " WHERE name REGEXP ?1 OR name || '-' || version REGEXP ?1";
                  else
                     return " WHERE origin REGEXP ?1";
                  end if;
               else
                  return " WHERE name = ?1";
               end if;
            when MATCH_EXACT =>
               if case_sensitivity_is_on then
                  if Strings.IsBlank (checkuid) then
                     if Strings.IsBlank (checkorigin) then
                        return " WHERE name = ?1 OR (name = SPLIT_VERSION('name', ?1) AND "
                          & " version = SPLIT_VERSION('version', ?1))";
                     else
                        return " WHERE origin = ?1";
                     end if;
                  else
                     return " WHERE name = ?1";
                  end if;
               else
                  if Strings.IsBlank (checkuid) then
                     if Strings.IsBlank (checkorigin) then
                        return " WHERE name = ?1 COLLATE NOCASE OR "
                          & "(name = SPLIT_VERSION('name', ?1) COLLATE NOCASE AND "
                          & " version = SPLIT_VERSION('version', ?1))";
                     else
                        return " WHERE origin = ?1 COLLATE NOCASE";
                     end if;
                  else
                     return " WHERE name = ?1 COLLATE NOCASE";
                  end if;
               end if;
         end case;
      end;
   end get_pattern_query;


end Core.Database;
