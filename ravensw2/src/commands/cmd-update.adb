--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;

with Core.Strings;
with Core.Database.Operations;
with Core.Repo.Operations;
with Core.Config;
with Core.Event;

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
      procedure update (Position : Repo.Active_Repository_Name_Set.Cursor);
      function update_successful (reponame : String) return Boolean;

      total_count : Natural := 0;
      all_okay    : Boolean := True;
      active      : Repo.Active_Repository_Name_Set.Vector;

      function update_successful (reponame : String) return Boolean
      is
         rc : Action_Result;
      begin
         Event.emit_message ("Updating " & reponame & " repository catalog...");

         rc := Repo.Operations.update_repository (reponame, force);

         total_count := total_count + 1;
         if rc = RESULT_UPTODATE then
            if not quiet then
               Event.emit_message (reponame & " repository is up to date.");
            end if;
         else
            if rc /= RESULT_OK then
               if not quiet then
                  Event.emit_message
                    ("Encountered an error updating the " & reponame & " repository!");
               end if;
               return False;
            end if;
         end if;
         return True;
      end update_successful;

      procedure update (Position : Repo.Active_Repository_Name_Set.Cursor)
      is
         rname : Text renames Repo.Active_Repository_Name_Set.Element (Position);
      begin
         if not update_successful (USS (rname)) then
            all_okay := False;
         end if;
      end update;

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
         active := Repo.ordered_active_repositories;
      else
         active.Append (SUS (reponame));
      end if;

      active.Iterate (update'Access);
      if not quiet and then total_count > 1 then
         if all_okay then
            Event.emit_message ("All active repositories are up to date.");
         else
            Event.emit_message ("Error updating repositories!");
         end if;
      end if;

      if all_okay or else not strict then
         return RESULT_OK;
      else
         return RESULT_FATAL;
      end if;
   end pkgcli_update;

end Cmd.Update;
