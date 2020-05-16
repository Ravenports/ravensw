--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Environment_Variables;
with Ada.Directories;

with Core.Strings;
with Core.Event;
with Core.Unix;

use Core.Strings;

package body Core.Database is

   package ENV renames Ada.Environment_Variables;
   package DIR renames Ada.Directories;

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

         if not IsBlank (pattern) then
            if contains (pattern, tilda) then
               checkuid := SUS (tilda & part_2 (pattern, tilda));
            elsif contains (pattern, slash) then
               checkorigin := SUS (slash & part_2 (pattern, slash));
            end if;
         end if;

         case match_style is
            when MATCH_ALL =>
               return "";  --  Will never reach here
            when MATCH_CONDITION =>
               return pattern;
            when MATCH_GLOB =>
               if IsBlank (checkuid) then
                  if IsBlank (checkorigin) then
                     return " WHERE name GLOB ?1 OR name || '-' || version GLOB ?1";
                  else
                     return " WHERE origin GLOB ?1";
                  end if;
               else
                  return " WHERE name = ?1";
               end if;
            when MATCH_REGEX =>
               if IsBlank (checkuid) then
                  if IsBlank (checkorigin) then
                     return " WHERE name REGEXP ?1 OR name || '-' || version REGEXP ?1";
                  else
                     return " WHERE origin REGEXP ?1";
                  end if;
               else
                  return " WHERE name = ?1";
               end if;
            when MATCH_EXACT =>
               if case_sensitivity_is_on then
                  if IsBlank (checkuid) then
                     if IsBlank (checkorigin) then
                        return " WHERE name = ?1 OR (name = SPLIT_VERSION('name', ?1) AND "
                          & " version = SPLIT_VERSION('version', ?1))";
                     else
                        return " WHERE origin = ?1";
                     end if;
                  else
                     return " WHERE name = ?1";
                  end if;
               else
                  if IsBlank (checkuid) then
                     if IsBlank (checkorigin) then
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


   --------------------------------------------------------------------
   --  rdb_security_status
   --------------------------------------------------------------------
   function rdb_security_status (path : String;
                                 install_as_user : Boolean) return Action_Result
   is
      fileowner : Unix.uid_t;
      filegroup : Unix.uid_t;
      sb        : aliased Unix.struct_stat;
   begin
      if install_as_user then
         fileowner := Unix.geteuid;
         filegroup := Unix.getegid;
      else
         fileowner := Unix.uid_t (0);
         filegroup := Unix.uid_t (0);
      end if;

      if not Unix.stat_ok (path, sb'Unchecked_Access) then
         if Unix.last_error_ACCESS then
            return RESULT_ENOACCESS;
         elsif Unix.last_error_NOENT then
            return RESULT_ENODB;
         else
            return RESULT_FATAL;
         end if;
      end if;

      --  if fileowner == 0, root ownership and no group or other
      --  read access.  if fileowner != 0, require no other read
      --  access and group read access IFF the group ownership == filegroup

      if Unix.bad_perms (fileowner, filegroup, sb'Unchecked_Access) then
         Event.emit_error (path & " permissions too lax");
         return RESULT_INSECURE;
      end if;

      if Unix.wrong_owner (fileowner, filegroup, sb'Unchecked_Access) then
         Event.emit_error
           (path & " wrong user or group ownership (expected " &
              int2str (Integer (fileowner)) & "/" & int2str (Integer (filegroup)) & ")");
         return RESULT_INSECURE;
      end if;

      return RESULT_OK;
   end rdb_security_status;


   --------------------------------------------------------------------
   --  check_access
   --------------------------------------------------------------------
   function check_access
     (mode   : RDB_Mode_Flags;
      dbdir  : String;
      dbname : String)
      return Action_Result
   is
      function make_dbpath return String;
      function user_can_install return Boolean;

      function make_dbpath return String is
      begin
         if IsBlank (dbname) then
            return dbdir;
         else
            return dbdir & "/" & dbname;
         end if;
      end make_dbpath;

      function user_can_install return Boolean is
      begin
         declare
            val  : String := ENV.Value ("INSTALL_AS_USER");
         begin
            return True;
         end;
      exception
         when Constraint_Error =>
            return False;
      end user_can_install;

      dbpath          : constant String := make_dbpath;
      install_as_user : constant Boolean := user_can_install;
      retval          : Action_Result;
      database_exists : Boolean;
      flags           : Unix.T_Access_Flags := (others => False);
   begin
      if (mode and RDB_MODE_READ) > 0 then
         flags.flag_read := True;
      end if;
      if (mode and RDB_MODE_WRITE) > 0 then
         flags.flag_write := True;
      end if;

      retval := rdb_security_status (dbpath, install_as_user);
      database_exists := (retval /= RESULT_ENODB);

      if database_exists then
         if retval /= RESULT_OK then
            return retval;
         end if;

         case mode is
            when RDB_MODE_EXISTS
               | RDB_MODE_READ
               | RDB_MODE_WRITE
               | RDB_MODE_READWRITE =>
               --  RDB_MODE_EXISTS is redundant, already known with database_exists
               if not Unix.valid_permissions (dbpath, flags) then
                  return RESULT_ENOACCESS;
               end if;
            when RDB_MODE_CREATE
               | RDB_MODE_CREATE or RDB_MODE_READ
                 | RDB_MODE_CREATE or RDB_MODE_WRITE
                 | RDB_MODE_ALL =>
               --  RDB_MODE_CREATE should only be called on directories
               return RESULT_FATAL;
         end case;
         return RESULT_OK;

      else
         --  database doesn't exist.  Assume it's intended to be called on a directory or
         --  to create a directory.

         case mode is
            when RDB_MODE_EXISTS
               | RDB_MODE_READ =>
               if not Unix.valid_permissions (dbdir, flags) then
                  return RESULT_ENOACCESS;
               end if;
            when RDB_MODE_WRITE
               | RDB_MODE_READ or RDB_MODE_WRITE =>
               if not Unix.valid_permissions (dbdir, flags) then
                  if Unix.last_error_NOENT then
                     begin
                        DIR.Create_Path (dbdir);
                        if not Unix.valid_permissions (dbdir, flags) then
                           return RESULT_ENOACCESS;
                        end if;
                     exception
                        when others =>
                           return RESULT_ENOACCESS;
                     end;
                  else
                     return RESULT_ENOACCESS;
                  end if;
               end if;
            when others =>
               --  the create routine will handle creating the directories
               null;
         end case;
         return RESULT_OK;
      end if;
   end check_access;


end Core.Database;
