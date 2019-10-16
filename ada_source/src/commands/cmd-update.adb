--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;

with Core.Strings; use Core.Strings;
with Core.PkgDB;
with Core.Config;
with Core.Repo_Operations;

package body Cmd.Update is

   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------
   --  pkgcli_update
   --------------------------------------------------------------------
   function pkgcli_update (force  : Boolean;
                           strict : Boolean;
                           quiet  : Boolean;
                           reponame : String) return Pkg_Error_Type
   is
      function update_without_issue (reponame : String) return Boolean;
      procedure update (position : pkg_repos_priority_crate.Cursor);

      update_count : Natural := 0;
      total_count  : Natural := 0;
      retcode      : Pkg_Error_Type := EPKG_OK;

      function update_without_issue (reponame : String) return Boolean
      is
         rc      : Pkg_Error_Type;
         repokey : Text := SUS (reponame);
         repo    : T_pkg_repo renames Config.repositories.Element (repokey);
      begin
         if not repo.enable then
            return True;
         end if;
         TIO.Put_Line ("Updating " & reponame & " repository catalog...");

         rc := Repo_Operations.Ops (repo.ops_variant).repo_update (repokey, force);

         total_count := total_count + 1;
         if rc = EPKG_UPTODATE then
            if not quiet then
               TIO.Put_Line (reponame & " repository is up to date.");
            end if;
            update_count := update_count + 1;
            return True;
         else
            if not quiet then
               TIO.Put_Line ("Encountered an error updating the " & reponame & " repository!");
            end if;
            --  Save the first encountered error (in case mode is not strict)
            if retcode /= EPKG_OK then
               retcode := rc;
            end if;
            return False;
         end if;
      end update_without_issue;

      procedure update (position : pkg_repos_priority_crate.Cursor)
      is
         key : Text renames pkg_repos_priority_crate.Element (position).reponame;
      begin
         if update_without_issue (USS (key)) then
            null;
         end if;
      end update;

      use type PkgDB.PkgDB_Mode_Flags;
   begin
      --  Only auto update if the user has write access.
      if PkgDB.pkgdb_access (mode  => (PkgDB.PKGDB_MODE_READ and PkgDB.PKGDB_MODE_WRITE),
                             dtype => PkgDB.PKGDB_DB_REPO) = EPKG_ENOACCESS
      then
         return EPKG_OK;
      end if;

      if Integer (Config.repositories.Length) = 0 then
         TIO.Put_Line (TIO.Standard_Error, "No active remote repositories configured");
         return EPKG_FATAL;
      end if;

      if Config.pkg_repos_activated_count = 0 then
         TIO.Put_Line ("No repositories are enabled");
         return EPKG_FATAL;
      end if;

      if IsBlank (reponame) then
         Config.repositories_order.Iterate (update'Access);
         if update_count = total_count then
            if not quiet then
               TIO.Put_Line ("All active repositories are up to date.");
            end if;
            return EPKG_OK;
         else
            if not quiet and then total_count > 1 then
               TIO.Put_Line ("Error updating repositories!");
            end if;
            if strict then
               return EPKG_FATAL;
            else
               return retcode;
            end if;
         end if;
      else
         if update_without_issue (reponame) then
            return EPKG_OK;
         else
            if strict then
               return EPKG_FATAL;
            else
               return retcode;
            end if;
         end if;
      end if;

   end pkgcli_update;

end Cmd.Update;
