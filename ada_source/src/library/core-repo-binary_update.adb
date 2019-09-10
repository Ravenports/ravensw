--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;
with Core.Config;
with Core.Event;
with Core.Repo_Operations;
with Core.Repo.Common;
with SQLite;

use Core.Strings;
use Core.Repo.Common;

package body Core.Repo.Binary_Update is

   --------------------------------------------------------------------
   --  pkg_repo_binary_update
   --------------------------------------------------------------------
   function pkg_repo_binary_update (reponame : Text; force : Boolean) return Pkg_Error_Type
   is
      finish_sql : String := "DROP TABLE repo_update;";
      got_meta   : Boolean := False;
      realforce  : Boolean := force;
      S_reponame : constant String := USS (reponame);
      dbdir      : constant String := Config.pkg_config_get_string (Config.conf_dbdir);
      dbpath     : constant String := dbdir & "/" & pkg_repo_binary_get_filename (S_reponame);
      metapath   : constant String := dbdir & "/" & pkg_repo_binary_get_meta (S_reponame);
      RW         : constant mode_t := ACCESS_R_OK + ACCESS_W_OK;
      mtime      : Unix.T_epochtime := 0;
      rbu_sb     : aliased Unix.struct_stat;
      repo       : T_pkg_repo renames Config.repositories.Element (reponame);
   begin
      if not repo.enable then
         return EPKG_OK;
      end if;

      if not SQLite.initialize_sqlite then
         return EPKG_FATAL;
      end if;

      Event.pkg_debug (1, "PkgRepo: verifying update for " & S_reponame);

      --  First of all, try to open and init repo and check whether it is fine
      if Repo_Operations.Ops (repo.ops_variant).repo_open (reponame, RW) then
         if not Repo_Operations.Ops (repo.ops_variant).repo_close (reponame, False) then
            return EPKG_FATAL;
         end if;
         if not realforce then
            if Unix.stat_ok (metapath, rbu_sb'Unchecked_Access) then
               got_meta := True;
               mtime := Unix.get_mtime (rbu_sb'Unchecked_Access);
            end if;
            if not got_meta then
               if Unix.stat_ok (dbpath, rbu_sb'Unchecked_Access) then
                  mtime := Unix.get_mtime (rbu_sb'Unchecked_Access);
               end if;
            end if;
         end if;
      else
         Event.pkg_debug (1, "PkgRepo: need forced update of " & S_reponame);
         realforce := True;
      end if;

      declare
         use type Unix.T_epochtime;

         msg : Text;
         res : Pkg_Error_Type;
      begin
         res := pkg_repo_binary_update_proceed (dbpath   => dbpath,
                                                reponame => S_reponame,
                                                mtime    => mtime,
                                                force    => realforce);

         if res /= EPKG_OK and then res /= EPKG_UPTODATE then
            Event.pkg_emit_notice (SUS ("Unable to update repository " & S_reponame));
         else
            if res = EPKG_OK then
               --  finish updating repository
               if not SQLite.exec_sql (repo.sqlite_handle, finish_sql, msg) then
                  Event.pkg_emit_error (SUS ("pkg_repo_binary_update() failed finish: " &
                                          USS (msg)));
                  return EPKG_FATAL;
               end if;
            end if;
         end if;

         if res = EPKG_OK and then mtime /= 0 then
            --  Set mtime from http request if possible
            Unix.set_file_times (dbpath, mtime, mtime);
            if got_meta then
               Unix.set_file_times (metapath, mtime, mtime);
            end if;
         end if;

         --  opened by pkg_repo_binary_update_proceed -> pkg_repo_binary_init_update
         if not Repo_Operations.Ops (repo.ops_variant).repo_close (reponame, False) then
            return EPKG_FATAL;
         end if;

         return res;
      end;
   end pkg_repo_binary_update;


   --------------------------------------------------------------------
   --  pkg_repo_binary_update_proceed
   --------------------------------------------------------------------
   function pkg_repo_binary_update_proceed
     (dbpath   : String;
      reponame : String;
      mtime    : in out Unix.T_epochtime;
      force    : Boolean) return Pkg_Error_Type is
   begin
      Event.pkg_debug (1, "Pkgrepo, begin update of '" & reponame & "'");
      if force then
         mtime := 0;
      end if;

      return EPKG_FATAL;
   end pkg_repo_binary_update_proceed;


   --------------------------------------------------------------------
   --  pkg_repo_binary_init_update
   --------------------------------------------------------------------
   function pkg_repo_binary_init_update (reponame : String) return Boolean
   is
      update_check_sql : constant String := "INSERT INTO repo_update VALUES(1);";
      update_start_sql : constant String := "CREATE TABLE IF NOT EXISTS repo_update (n INT);";
      repokey          : Text := SUS (reponame);
      RW   : constant mode_t := ACCESS_R_OK + ACCESS_W_OK;
      repo : T_pkg_repo renames Config.repositories.Element (repokey);
      msg  : Text;
   begin

      --   [Re]create repo
      if not Repo_Operations.Ops (repo.ops_variant).repo_create (repokey) then
         Event.pkg_emit_notice (SUS ("Unable to create repository " & reponame));
         return False;
      end if;

      if not Repo_Operations.Ops (repo.ops_variant).repo_open (repokey, RW) then
         Event.pkg_emit_notice (SUS ("Unable to open created repository " & reponame));
         return False;
      end if;

      if not Repo_Operations.Ops (repo.ops_variant).repo_init (repokey) then
         Event.pkg_emit_notice (SUS ("Unable to initialize repository " & reponame));
         return False;
      end if;

      if not SQLite.exec_sql (repo.sqlite_handle, update_check_sql, msg) then
         Event.pkg_emit_notice (SUS ("Previous update may not been finished: " & USS (msg)));
         return False;
      else
         if not SQLite.exec_sql (repo.sqlite_handle, update_start_sql, msg) then
            Event.pkg_emit_notice (SUS ("pkg_repo_binary_init_update SQL: " & USS (msg)));
            return False;
         end if;
      end if;

      return True;

   end pkg_repo_binary_init_update;

end Core.Repo.Binary_Update;
