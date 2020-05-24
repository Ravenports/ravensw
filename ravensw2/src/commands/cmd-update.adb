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
      function update_successful (reponame : String) return Boolean;

      update_count : Natural := 0;
      total_count  : Natural := 0;
      retcode      : Action_Result := RESULT_OK;

      function update_successful (reponame : String) return Boolean
      is
         rc : Action_Result;
      begin
         Event.emit_message ("Updating " & reponame & " repository catalog...");

         --  rc := Repo.Operations.update_repository (reponame);
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
            if retcode = RESULT_OK then
               return True;
            else
               retcode := rc;
               return False;
            end if;
         end if;
      end update_successful;

   begin
      --  Only auto update if the user has write access.
      if Database.Operations.database_access
        (mode  => Database.RDB_MODE_ALL,
         dtype => Database.RDB_DB_REPO) = RESULT_ENOACCESS
      then
         return RESULT_OK;
      end if;

      if Repo.total_repositories = 0 then
         Event.emit_error ("No repositories configured");
         return RESULT_FATAL;
      end if;

      if IsBlank (reponame) then
         if Repo.count_of_active_repositories = 0 then
            Event.emit_message ("No repositories are enabled");
            return RESULT_FATAL;
         end if;
      elsif not Repo.repository_is_active (reponame) then
         Event.emit_message ("The " & reponame & " repository is not active");
         return RESULT_FATAL;
      end if;

      if IsBlank (reponame) then
         declare
            procedure update (Position : Repo.Active_Repository_Name_Set.Cursor);

            active : Repo.Active_Repository_Name_Set.Vector := Repo.ordered_active_repositories;

            procedure update (Position : Repo.Active_Repository_Name_Set.Cursor)
            is
               rname : Text renames Repo.Active_Repository_Name_Set.Element (Position);
               succ  : Boolean;
            begin
               succ := update_successful (USS (rname));
            end update;
         begin
            active.Iterate (update'Access);
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
         if update_successful (reponame) then
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
