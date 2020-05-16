--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;

with Core.Strings;
with Core.Database.Operations;
with Core.Config;
with Core.Event;
with Core.Repo;

use Core.Strings;

package body Cmd.Update is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  pkgcli_update
   --------------------------------------------------------------------
   function pkgcli_update (force    : Boolean;
                           strict   : Boolean;
                           quiet    : Boolean;
                           reponame : String) return Action_Result
   is
      function success_update (reponame : String) return Boolean;
--        procedure update (position : pkg_repos_priority_crate.Cursor);

      update_count : Natural := 0;
      total_count  : Natural := 0;
      retcode      : Action_Result := RESULT_OK;

      function success_update (reponame : String) return Boolean
      is
         rc : Action_Result;
      begin
         Event.emit_message ("Updating " & reponame & " repository catalog...");

         --  TODO:  Implement repo update operation
         --  rc := Repo_Operations.Ops (repo.ops_variant).repo_update (repokey, force);
         rc := RESULT_UPTODATE;

         total_count := total_count + 1;
         if rc = RESULT_UPTODATE then
            if not quiet then
               Event.emit_message
                 (reponame & " repository is up to date.");
            end if;
            update_count := update_count + 1;
            return True;
         else
            if not quiet then
               Event.emit_message
                 ("Encountered an error updating the " & reponame & " repository!");
            end if;
            --  Save the first encountered error (in case mode is not strict)
            if retcode /= RESULT_OK then
               retcode := rc;
            end if;
            return False;
         end if;
      end success_update;

   begin
      --  Only auto update if the user has write access.
      if Database.Operations.database_access
        (mode  => Database.RDB_MODE_ALL,
         dtype => Database.RDB_DB_REPO) = RESULT_ENOACCESS
      then
         return RESULT_OK;
      end if;

      if Repo.total_repositories = 0 then
         Event.emit_error ("No active remote repositories configured");
         return RESULT_FATAL;
      end if;

      if Repo.count_of_active_repositories = 0 or else
        (not IsBlank (reponame) and then not Repo.repository_is_active (reponame))
      then
         Event.emit_message ("No repositories are enabled");
         return RESULT_FATAL;
      end if;

      if IsBlank (reponame) then
         declare
            list  : String := Repo.joined_priority_order;
            num   : Natural := count_char (list, LAT.LF) + 1;
            delim : String (1 .. 1) := (others => LAT.LF);
         begin
            for x in 1 .. num loop
               declare
                  rname : String := specific_field (list, x, delim);
                  succ  : Boolean;
               begin
                  succ := success_update (rname);
               end;
            end loop;
         end;
         if update_count = total_count then
            if not quiet then
               Event.emit_message ("All active repositories are up to date.");
            end if;
            return RESULT_OK;
         else
            if not quiet and then total_count > 1 then
               Event.emit_message ("Error updating repositories!");
            end if;
            if strict then
               return RESULT_FATAL;
            else
               return retcode;
            end if;
         end if;
      else
         if success_update (reponame) then
            return RESULT_OK;
         else
            if strict then
               return RESULT_FATAL;
            else
               return retcode;
            end if;
         end if;
      end if;
   end pkgcli_update;

end Cmd.Update;
