--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Directories;
with Ada.Text_IO;

with Core.Event;
with Core.Context;
with Core.Repo.Meta;
with Core.Repo.Fetch;
with Core.Repo.Operations.Schema;
with Core.Repo.Iterator.Packages;
with Core.VFS;
with Core.Database.CustomCmds;
with Core.CommonSQL;
with Core.Checksum;
with Core.Config;
with SQLite;


package body Core.Repo.Operations is

   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------
      --  close_repository
   --------------------------------------------------------------------
   procedure close_repository (reponame : Text; commit : Boolean)
   is
      procedure close_database (key : Text; Element : in out A_repo);
      procedure close_database (key : Text; Element : in out A_repo)
      is
         repository : A_repo renames Element;
      begin
         Schema.prstmt_finalize (repository.sqlite_handle);
         SQLite.close_database (repository.sqlite_handle);
         repository.sqlite_handle := null;
      end close_database;
   begin
      if repositories.Contains (reponame) then

         if not SQLite.db_connected (repositories.Element (reponame).sqlite_handle) then
            return;
         end if;

         if commit then
            if not CommonSQL.transaction_commit
              (db        => repositories.Element (reponame).sqlite_handle,
               srcfile   => internal_srcfile,
               func      => "close_repository",
               savepoint => "")
            then
               Event.emit_error ("close_repository(): Failed to commit transaction");
               return;
            end if;
         end if;

         repositories.Update_Element (Position => repositories.Find (reponame),
                                      Process  => close_database'Access);
      else
         raise invalid_repo_name;
      end if;
   end close_repository;


   --------------------------------------------------------------------
   --  close_all_open_repositories
   --------------------------------------------------------------------
   procedure close_all_open_repositories
   is
      procedure close (position : Repos_Priority_Crate.Cursor);
      procedure close (position : Repos_Priority_Crate.Cursor) is
      begin
         close_repository (Repos_Priority_Crate.Element (position).reponame, False);
      end close;
   begin
      repositories_open.Iterate (close'Access);
      repositories_open.Clear;
   end close_all_open_repositories;


   --------------------------------------------------------------------
   --  sqlite_filename
   --------------------------------------------------------------------
   function sqlite_filename (reponame : String) return String is
   begin
      --  TODO: Change extension back to ".sqlite" before releasing into production
      return reponame & ".sqlite.dev";
   end sqlite_filename;


   --------------------------------------------------------------------
   --  open_repository
   --------------------------------------------------------------------
   function open_repository (reponame : String; readonly : Boolean) return Action_Result
   is
      --  Ensure that SQLite is initialized with syscall_override before calling this

      procedure open_database (key : Text; Element : in out A_repo);

      func    : constant String := "open_repository";
      dbdirfd : Unix.File_Descriptor;
      result  : Action_Result := RESULT_FATAL;

      procedure open_database (key : Text; Element : in out A_repo)
      is
         repository : A_repo renames Element;
         errprefix  : constant String := "Repository " & reponame & " load error: ";

         fd         : Unix.File_Descriptor;
         filename   : constant String := meta_filename (reponame);
         dbfile     : constant String := sqlite_filename ("repo-" & reponame);
         flags      : constant Unix.T_Open_Flags := (RDONLY => True, others => False);
         success    : Action_Result;
         tmp        : Repo_metadata;
      begin
         fd := Unix.open_file (dirfd         => dbdirfd,
                               relative_path => filename,
                               flags         => flags);

         if Unix.file_connected (fd) then
            tmp := Repo.Meta.meta_load (fd, success);
            if success = RESULT_OK then
               repository.meta := tmp;
            else
               Event.emit_errno (errprefix & "openat", "dbdirfd, " & filename, Unix.errno);
               if Unix.close_file (fd) then
                  null;
               end if;
               return;
            end if;
            if not Unix.close_file (fd) then
               Event.emit_errno (errprefix & "close_file", "meta fd", Unix.errno);
               return;
            end if;
         else
            Event.emit_errno (errprefix & "open_file", filename, Unix.errno);
            return;
         end if;

         if not Unix.relative_file_readable (dbdirfd, dbfile) then
            Event.emit_error (dbfile & " is not readable");
            return;
         end if;

         --  main open routine
         declare
            opened : Boolean;
         begin
            if readonly then
               opened := SQLite.open_sqlite_database_readonly
                 (path => dbfile,
                  ppDB => repository.sqlite_handle'Access);
            else
               opened := SQLite.open_sqlite_database_readwrite
                 (path => dbfile,
                  ppDB => repository.sqlite_handle'Access);
            end if;
            if not opened then
               if SQLite.database_corrupt (repository.sqlite_handle) then
                  Event.emit_error
                    ("Database corrupt.  Are you running on NFS?  " &
                       "If so, ensure the locking mechanism is properly set up.");
               end if;
               Event.emit_errno (errprefix & "open sqlite", dbfile, Unix.errno);
               return;
            end if;
         end;

         --  Verify database is usable
         declare
            res_int64 : int64;
            sql       : constant String := "SELECT count(name) FROM sqlite_master " &
                                           "WHERE type='table' AND name='repodata'";
         begin
            if CommonSQL.get_int64 (db      => repository.sqlite_handle,
                                    srcfile => internal_srcfile,
                                    func    => func,
                                    sql     => sql,
                                    res     => res_int64,
                                    silence => False) /= RESULT_OK
            then
               Event.emit_errno (errprefix & "name count", sql, Unix.errno);
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;

            if res_int64 /= 1 then
               Event.emit_error
                 ("Repository " & reponame &
                    " contains no repodata table, database must be recreated");
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;
         end;

         --
         declare
            res_int64 : int64;
            url       : constant String := repo_url (repository);
            sql       : constant String := "SELECT count(key) from repodata " &
                        "WHERE key = " & SQ ("packagesite") & " and value = " & SQ (url);
         begin
            if CommonSQL.get_int64 (db      => repository.sqlite_handle,
                                    srcfile => internal_srcfile,
                                    func    => func,
                                    sql     => sql,
                                    res     => res_int64,
                                    silence => False) /= RESULT_OK
            then
               Event.emit_errno (errprefix & "key count", sql, Unix.errno);
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;

            if res_int64 /= 1 then
               Event.emit_error ("Repository " & reponame &
                                   " has a wrong packagesite, database must be recreated");
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;
         end;

         --  Upgrade as necessary
         declare
            upres : Action_Result;
         begin
            upres := Schema.repo_upgrade (repository.sqlite_handle, reponame);
            case upres is
               when RESULT_UPTODATE => null;
               when RESULT_OK       => null;
               when RESULT_REPOSCHEMA
                  | RESULT_FATAL =>
                  Event.emit_error ("Repository " & reponame & " has an unsupported schema");
                  Event.emit_error ("The database must be recreated.");
                  SQLite.close_database (repository.sqlite_handle);
                  if not readonly then
                     begin
                        DIR.Delete_File (dbfile);
                     exception
                        when others =>
                           Event.emit_error ("Failed to unlink " & dbfile & "!");
                     end;
                  end if;
                  return;
               when others =>
                  null;  --  shouldn't happen
            end case;
         end;

         --  Check digests format
         declare
            my_pkg : aliased Pkgtypes.A_Package;
            it     : Repo.Iterator.Packages.SQLite_Iterator;
         begin
            if it.initialize_as_standard_query (reponame => reponame,
                                                pattern  => "",
                                                match    => Database.MATCH_ALL,
                                                just_one => True) /= RESULT_OK
            then
               Event.emit_error ("Failed to initialize SQLite pkg iterator");
               SQLite.close_database (repository.sqlite_handle);
               return;
            end if;

            --  Next can be OK/END/FATAL
            case it.Next (pkg_access => my_pkg'Unchecked_Access,
                          sections   => (Pkgtypes.basic => True, others => False))
            is
               when RESULT_END =>
                  null;
               when RESULT_OK =>
                  if IsBlank (my_pkg.digest) or else
                    not Checksum.checksum_is_valid (my_pkg.digest)
                  then
                     Event.emit_error ("The " & reponame &
                                         " repository has an incompatible checksum format");
                     Event.emit_error ("The database must be recreated.");
                     SQLite.close_database (repository.sqlite_handle);
                     return;
                  end if;
               when others =>
                  Event.emit_error ("Failed to retrieve package in open_repository()");
                  SQLite.close_database (repository.sqlite_handle);
                  return;
            end case;
         end;
         result := RESULT_OK;
      end open_database;

   begin
      Event.emit_debug (3, "open_repository " & SQ (reponame));
      if repositories.Contains (SUS (reponame)) then
         dbdirfd := Context.reveal_db_directory_fd;
         repositories.Update_Element (repositories.Find (SUS (reponame)), open_database'Access);
         return result;
      else
         Event.emit_debug (3, "open_repository invalid! " & SQ (reponame));
         raise invalid_repo_name;
      end if;
   end open_repository;


   --------------------------------------------------------------------
   --  initialize_repository
   --------------------------------------------------------------------
   function initialize_repository (reponame : Text) return Action_Result
   is
      procedure make_it_so (key : Text; Element : in out A_repo);

      result : Action_Result;

      procedure make_it_so (key : Text; Element : in out A_repo)
      is
         db : sqlite_h.sqlite3_Access renames Element.sqlite_handle;
         onward : Boolean := True;
      begin
         --  Impossible to fail
         Database.CustomCmds.define_file_exists (db);

         if CommonSQL.exec (db, "PRAGMA synchronous=default") /= RESULT_OK then
            onward := False;
         end if;

         if onward then
            if CommonSQL.exec (db, "PRAGMA foreign_keys=on") /= RESULT_OK then
               onward := False;
            end if;
         end if;

         if onward then
            Database.CustomCmds.define_six_functions (db);
            if Schema.prstmt_initialize (db) /= RESULT_OK then
               onward := False;
            end if;
         end if;

         case onward is
            when True  => result := RESULT_OK;
            when False => result := RESULT_FATAL;
         end case;
      end make_it_so;

   begin
      if repositories.Contains (reponame) then
         repositories.Update_Element (repositories.Find (reponame), make_it_so'Access);
         return result;
      else
         raise invalid_repo_name;
      end if;
   end initialize_repository;


   --------------------------------------------------------------------
   --  create_repository
   --------------------------------------------------------------------
   function create_repository (reponame : String) return Action_Result
   is
      procedure make_it_so (key : Text; Element : in out A_repo);

      dbfile : constant String := sqlite_filename (reponame);
      dir_fd : Unix.File_Descriptor;
      result : Action_Result := RESULT_FATAL;

      procedure make_it_so (key : Text; Element : in out A_repo)
      is
         db     : sqlite_h.sqlite3_Access renames Element.sqlite_handle;
         opened : Boolean;
      begin
         opened := SQLite.open_sqlite_database_readwrite (path => dbfile, ppDB => db'Access);
         if not opened then
            if SQLite.database_corrupt (db) then
               Event.emit_error
                 ("Database corrupt.  Are you running on NFS?  " &
                    "If so, ensure the locking mechanism is properly set up.");
            end if;
            Event.emit_errno ("sqlite3_open", dbfile, Unix.errno);
            return;
         end if;
         if Schema.import_schema_2013 (db) /= RESULT_OK then
            SQLite.close_database (db);
            return;
         end if;
         if Schema.repo_upgrade (db, reponame) /= RESULT_OK then
            SQLite.close_database (db);
            return;
         end if;
         declare
            stmt : aliased sqlite_h.sqlite3_stmt_Access;
            sql1 : constant String :=
              "CREATE TABLE IF NOT EXISTS repodata ("
              & "  key TEXT UNIQUE NOT NULL,"
              & "  value TEXT NOT NULL"
              & ");";
            sql2 : constant String :=
              "INSERT OR REPLACE INTO repodata (key, value)"
              & " VALUES (" & DQ ("packagesite") & ", ?1);";
         begin
            if CommonSQL.exec (db, sql1) /= RESULT_OK then
               Event.emit_error ("Unable to register the packagesite in the database");
               SQLite.close_database (db);
               return;
            end if;
            if not SQLite.prepare_sql (db, sql2, stmt'Access) then
               CommonSQL.ERROR_SQLITE (db, internal_srcfile, "create_repository", sql2);
               SQLite.close_database (db);
               return;
            end if;
            SQLite.bind_string (stmt, 1, USS (Element.url));
            if SQLite.step_to_completion (stmt) then
               SQLite.finalize_statement (stmt);
            else
               CommonSQL.ERROR_SQLITE
                 (db, internal_srcfile, "create_repository", "step through " & sql2);
               SQLite.finalize_statement (stmt);
               SQLite.close_database (db);
               return;
            end if;
            SQLite.close_database (db);
            result := RESULT_OK;
         end;
      end make_it_so;

   begin
      if repositories.Contains (SUS (reponame)) then
         dir_fd := Context.reveal_db_directory_fd;
         if not SQLite.initialize_sqlite then
            return RESULT_ENODB;
         end if;
         SQLite.rdb_syscall_overload;
         repositories.Update_Element (repositories.Find (SUS (reponame)), make_it_so'Access);
         return result;
      else
         raise invalid_repo_name;
      end if;
   end create_repository;


   --------------------------------------------------------------------
   --  check_repository_access
   --------------------------------------------------------------------
   function check_repository_access
     (reponame : String;
      mode     : Database.RDB_Mode_Flags) return Action_Result
   is
      db_dir : String := Config.configuration_value (Config.dbdir);
   begin
      return Database.check_access (mode   => mode,
                                    dbdir  => db_dir,
                                    dbname => sqlite_filename (reponame));
   end check_repository_access;


   --------------------------------------------------------------------
   --  update_init
   --------------------------------------------------------------------
   function update_init (reponame : String) return Action_Result
   is
      check_sql : constant String := "INSERT INTO repo_update VALUES(1);";
      start_sql : constant String := "CREATE TABLE IF NOT EXISTS repo_update (n INT);";
      repo_key  : Text := SUS (reponame);
   begin
      if not repositories.Contains (repo_key) then
         raise invalid_repo_name;
      end if;

      --  [Re]create repo
      if create_repository (reponame) /= RESULT_OK then
         Event.emit_notice ("Unable to create repository " & reponame);
         return RESULT_FATAL;
      end if;

      if open_repository (reponame, False) /= RESULT_OK then
         Event.emit_notice ("Unable to open created repository " & reponame);
         return RESULT_FATAL;
      end if;

      if initialize_repository (repo_key) /= RESULT_OK then
         Event.emit_notice ("Unable to initialize repository " & reponame);
         return RESULT_FATAL;
      end if;

      declare
         db : sqlite_h.sqlite3_Access renames repositories.Element (repo_key).sqlite_handle;
      begin
         if CommonSQL.exec (db, check_sql) = RESULT_OK then
            Event.emit_notice ("Previous update has not been finished, restart it");
            return RESULT_END;
         else
            return CommonSQL.exec (db, start_sql);
         end if;
      end;
   end update_init;


   --------------------------------------------------------------------
   --  update_proceed
   --------------------------------------------------------------------
   function update_proceed
     (reponame : String;
      filepath : String;
      mtime    : in out Unix.T_epochtime;
      force    : Boolean) return Action_Result
   is
      local_time : Unix.T_epochtime;
      my_repo    : A_repo := get_repository (reponame);
      file_size  : int64;
      rc         : Action_Result := RESULT_FATAL;
      skip_rest  : Boolean := False;
   begin
      --  We know repository name is valid; we don't need to check again

      Event.emit_debug (1, "Proceed: begin update of " & DQ (filepath));
      if force then
         mtime := 0;
      end if;

      --
      --  fetch meta
      --
      local_time := mtime;
      if Repo.Fetch.fetch_meta (my_repo   => my_repo,
                                timestamp => local_time) = RESULT_FATAL
      then
         Event.emit_notice
           ("repository " & SQ (reponame) & " has no meta file, using default settings.");
      end if;

      --
      --  fetch packagesite
      --
      local_time := mtime;
      declare
         tmp_manifest : String :=
           Repo.Fetch.fetch_remote_extract_to_temporary_file
             (my_repo   => my_repo,
              filename  => USS (my_repo.meta.manifests),
              timestamp => local_time,
              file_size => file_size,
              retcode   => rc);

         in_trans : Boolean := False;
         db       : sqlite_h.sqlite3_Access renames my_repo.sqlite_handle;
         func     : constant String := "update_proceed";
         backup   : constant String := filepath & "-ravtmp";
         CIP      : constant String := "CREATE INDEX packages";
         silentrc : Action_Result;
      begin
         if rc = RESULT_OK then
            mtime := local_time;
         else
            skip_rest := True;
         end if;

         if not skip_rest then
            DIR.Copy_File (filepath, backup);
            rc := update_init (reponame);
            if rc /= RESULT_OK then
               rc := RESULT_FATAL;
               skip_rest := True;
            end if;
         end if;

         if not skip_rest then
            Event.emit_debug (1, "Proceed: reading new packagesite.yaml for " & SQ (reponame));
            Event.emit_progress_start ("Processing entries");

            --  200MB should be enough for mmap
            silentrc := CommonSQL.exec (db, "PRAGMA mmap_size = 209715200;");
            silentrc := CommonSQL.exec (db, "PRAGMA foreign_keys = OFF;");
            silentrc := CommonSQL.exec (db, "PRAGMA synchronous = OFF;");
            --  FreeBSD set PRAGMA page_size = getpagesize().
            --  It's unclear what the benefit is over the default 4Kb page size.
            --  Omit this PRAGMA for now

            if CommonSQL.transaction_begin (db, internal_srcfile, func, "REPO") then
               in_trans := True;
            else
               skip_rest := True;
            end if;
         end if;

         if not skip_rest then
            declare
               file_handle : TIO.File_Type;
               cnt         : Natural := 0;
               total_len   : int64 := 0;
            begin
               TIO.Open (File => file_handle,
                         Mode => TIO.In_File,
                         Name => tmp_manifest);
               while not TIO.End_Of_File (file_handle) loop
                  declare
                     line : constant String := TIO.Get_Line (file_handle);
                  begin
                     cnt := cnt + 1;
                     total_len := total_len + line'Length;
                     if (cnt mod 10) = 0 then
                        Event.emit_progress_tick (total_len, file_size);
                     end if;
                     rc := add_from_manifest (my_repo, line);
                     if rc = RESULT_OK then
                        Event.emit_incremental_update (reponame, cnt);
                     else
                        exit;
                     end if;
                  end;
               end loop;
               Event.emit_progress_tick (file_size, file_size);
               TIO.Close (file_handle);
            exception
               when others =>
                  if TIO.Is_Open (file_handle) then
                     TIO.Close (file_handle);
                  end if;
            end;
            if rc = RESULT_OK then
               silentrc := CommonSQL.exec
                 (db, CIP & "_origin ON packages(origin COLLATE NOCASE);"
                  & CIP & "_name ON packages(name COLLATE NOCASE);"
                  & CIP & "_uid_nocase ON packages(name COLLATE NOCASE, origin COLLATE NOCASE);"
                  & CIP & "_version_nocase ON packages(name COLLATE NOCASE, version);"
                  & CIP & "_uid ON packages(name, origin);"
                  & CIP & "_version ON packages(name, version);"
                  & "CREATE UNIQUE INDEX packages_digest ON packages(manifestdigest);"
                 );
            end if;
         end if;

         --
         --  CLEANUP
         --
         if in_trans then
            if rc /= RESULT_OK then
               if not CommonSQL.transaction_rollback (db, internal_srcfile, func, "REPO") then
                  null;
               end if;
            else
               if CommonSQL.transaction_commit (db, internal_srcfile, func, "REPO") then
                  rc := RESULT_FATAL;
               end if;
            end if;
         end if;

         --  restore the previous db in case of failures
         if rc /= RESULT_OK and then rc /= RESULT_UPTODATE then
            --  failure, so restore to previous database
            DIR.Delete_File (filepath);
            DIR.Rename (backup, filepath);
         else
            --  remove temporary backup
            DIR.Delete_File (backup);
         end if;
         DIR.Delete_File (tmp_manifest);

         return rc;
      end;
   end update_proceed;


   --------------------------------------------------------------------
   --  update_repository
   --------------------------------------------------------------------
   function update_repository (reponame : String; force : Boolean) return Action_Result
   is
      update_finish_sql : constant String := "DROP TABLE repo_update;";
      db_dir            : constant String := Config.configuration_value (Config.dbdir) & "/";
      path_to_dbfile    : constant String := db_dir & sqlite_filename (reponame);
      path_to_metafile  : constant String := db_dir & meta_filename (reponame);
      stamp             : Unix.T_epochtime := 0;
      got_meta          : Boolean := False;
      skip_next_step    : Boolean := False;
      res               : Action_Result;
      local_force       : Boolean := force;
   begin
      if not SQLite.initialize_sqlite then
         return RESULT_ENODB;
      end if;
      if not Repo.repository_is_active (reponame) then
         return RESULT_OK;
      end if;

      Event.emit_debug (1, "REPO: verifying update for " & reponame);

      --  First of all, try to open and init repo and check whether it is fine
      if open_repository (reponame, False) /= RESULT_OK then
         Event.emit_debug (1, "REPO: need forced update of " & reponame);
         local_force := True;
         stamp := 0;
      else
         close_repository (SUS (reponame), False);
         if DIR.Exists (path_to_metafile) then
            if local_force then
               stamp := 0;
               got_meta := True;
            else
               begin
                  stamp := Unix.get_file_modification_time (path_to_metafile);
                  got_meta := True;
               exception
                  when Unix.bad_stat =>
                     stamp := 0;
               end;
            end if;
         end if;
         if DIR.Exists (path_to_dbfile) then
            if not got_meta and then not local_force then
               begin
                  stamp := Unix.get_file_modification_time (path_to_dbfile);
               exception
                  when Unix.bad_stat =>
                     stamp := 0;
               end;
            end if;
         end if;
      end if;

      res := update_proceed (reponame, path_to_dbfile, stamp, local_force);
      if res /= RESULT_OK and then res /= RESULT_UPTODATE then
         Event.emit_notice ("Unable to update repository " & reponame);
         skip_next_step := True;
      end if;

      if not skip_next_step then
         if res = RESULT_OK then
            res := CommonSQL.exec (Repo.get_repository (reponame).sqlite_handle,
                                   update_finish_sql);
         end if;
      end if;

      --  Set mtime from http request if possible
      declare
         use type Unix.T_epochtime;
      begin
         if stamp /= 0 and then res = RESULT_OK then
            Unix.set_file_times (path        => path_to_dbfile,
                                 access_time => stamp,
                                 mod_time    => stamp);
            if got_meta then
               Unix.set_file_times (path        => path_to_metafile,
                                    access_time => stamp,
                                    mod_time    => stamp);
            end if;
         end if;
      end;
      close_repository (SUS (reponame), False);
      return res;
   end update_repository;


   --------------------------------------------------------------------
   --  add_from_manifest
   --------------------------------------------------------------------
   function add_from_manifest
     (repo     : A_repo;
      manifest : String) return Action_Result
   is
      my_pkg : aliased Pkgtypes.A_Package;
      abi    : Text;
   begin
      my_pkg.package_type := Pkgtypes.PKG_REMOTE;
      if Manifest.parse_manifest (my_pkg'Access, manifest) /= RESULT_OK then
         return RESULT_FATAL;
      end if;

      if IsBlank (my_pkg.digest) or else not Checksum.checksum_is_valid (my_pkg.digest) then
         -- TODO:  pkg_checksum_calculate(pkg, NULL);
         null;
      end if;

      --  Does ravensw ever run into a blank abi situation?  Might be a pkgng thing ...
      if IsBlank (my_pkg.abi) then
         abi := my_pkg.arch;
      else
         abi := my_pkg.abi;
      end if;

      if IsBlank (abi) or else not



	if (pkg->digest == NULL || !pkg_checksum_is_valid(pkg->digest, strlen(pkg->digest)))
		pkg_checksum_calculate(pkg, NULL);
	abi = pkg->abi != NULL ? pkg->abi : pkg->arch;
	if (abi == NULL || !is_valid_abi(abi, true)) {
		rc = EPKG_FATAL;
		pkg_emit_error("repository %s contains packages with wrong ABI: %s",
			repo->name, abi);
		goto cleanup;
	}
	if (!is_valid_os_version(pkg)) {
		rc = EPKG_FATAL;
		pkg_emit_error("repository %s contains packages for wrong OS "
		    "version: %s", repo->name, abi);
		goto cleanup;
	}

	free(pkg->reponame);
	pkg->reponame = xstrdup(repo->name);

	rc = pkg_repo_binary_add_pkg(pkg, NULL, sqlite, true);

cleanup:
	pkg_free(pkg);

	return (rc);
   end add_from_manifest;

end Core.Repo.Operations;
