--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Directories;

with Core.Event;
with Core.Context;
with Core.Repo.Meta;
with Core.Repo.Operations.Schema;
with Core.Repo.Iterator.Packages;
with Core.VFS;
with Core.Database.CustomCmds;
with Core.CommonSQL;
with Core.Config;
with SQLite;


package body Core.Repo.Operations is

   package DIR renames Ada.Directories;

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
              (repositories.Element (reponame).sqlite_handle, "")
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
   --  meta_filename
   --------------------------------------------------------------------
   function meta_filename (reponame : String) return String is
   begin
      return reponame & ".meta";
   end meta_filename;


   --------------------------------------------------------------------
   --  open_repository
   --------------------------------------------------------------------
   function open_repository (reponame : String; readonly : Boolean) return Action_Result
   is
      --  Ensure that SQLite is initialized with syscall_override before calling this

      procedure open_database (key : Text; Element : in out A_repo);

      dbdirfd : Unix.File_Descriptor;
      result  : Action_Result;

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
            use type sqlite_h.enum_error_types;
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
               if SQLite.get_last_error_code (repository.sqlite_handle) =
                 sqlite_h.SQLITE_CORRUPT
               then
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
            if CommonSQL.get_pragma (repository.sqlite_handle, sql, res_int64, False) /= RESULT_OK
            then
               Event.emit_errno (errprefix & "pragma", sql, Unix.errno);
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

         --  Check package site
         declare
            res_int64 : int64;
            url       : constant String := repo_url (repository);
            sql       : constant String := "select count(key) from repodata " &
                        "WHERE key = " & DQ ("packagesite") & " and value = " & SQ (url);
         begin
            if CommonSQL.get_pragma (repository.sqlite_handle, sql, res_int64, False) /= RESULT_OK
            then
               Event.emit_errno (errprefix & "pragma", sql, Unix.errno);
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
                                                match    => Database.MATCH_ALL) /= RESULT_OK
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
      if repositories.Contains (SUS (reponame)) then
         dbdirfd := Context.reveal_db_directory_fd;
         repositories.Update_Element (repositories.Find (SUS (reponame)), open_database'Access);
         return result;
      else
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
   --  initialize_repository
   --------------------------------------------------------------------
   function create_repository (reponame : String) return Action_Result
   is
      procedure make_it_so (key : Text; Element : in out A_repo);

      dbfile : constant String := sqlite_filename (reponame);
      dir_fd : Unix.File_Descriptor;
      result : Action_Result := RESULT_FATAL;

      procedure make_it_so (key : Text; Element : in out A_repo)
      is
         use type sqlite_h.enum_error_types;
         db     : sqlite_h.sqlite3_Access renames Element.sqlite_handle;
         opened : Boolean;
      begin
         opened := SQLite.open_sqlite_database_readwrite (path => dbfile, ppDB => db'Access);
         if not opened then
            if SQLite.get_last_error_code (db) = sqlite_h.SQLITE_CORRUPT
            then
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
            problem   : Boolean;
            row_found : Boolean;
         begin
            if CommonSQL.exec (db, sql1) /= RESULT_OK then
               Event.emit_error ("Unable to register the packagesite in the database");
               SQLite.close_database (db);
               return;
            end if;
            if not SQLite.prepare_sql (db, sql2, stmt'Access) then
               CommonSQL.ERROR_SQLITE (db, "create_repository", sql2);
               SQLite.close_database (db);
               return;
            end if;
            SQLite.bind_string (stmt, 1, USS (Element.url));
            row_found := SQLite.step_through_statement (stmt, problem);
            SQLite.finalize_statement (stmt);
            if problem then
               CommonSQL.ERROR_SQLITE (db, "create_repository", "step through " & sql2);
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
      if not repositories.Contains (SUS (reponame)) then
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
      mtime    : in out Unix.T_epochtime;
      force    : Boolean) return Action_Result
   is
      repo_key   : Text := SUS (reponame);
      local_time : Unix.T_epochtime;
   begin
      Event.emit_debug (1, "Proceed: begin update of " & SQ (reponame));
      if force then
         mtime := 0;
      end if;

      --
      --  fetch meta
      --
      local_time := mtime;

--        /* Fetch meta */
--  	local_t = *mtime;
--  	if (pkg_repo_fetch_meta(repo, &local_t) == EPKG_FATAL)
--  		pkg_emit_notice("repository %s has no meta file, using "
--  		    "default settings", repo->name);
--
--  	/* Fetch packagesite */
--  	local_t = *mtime;
--  	fd = pkg_repo_fetch_remote_extract_fd(repo,
--  		repo->meta->manifests, &local_t, &rc, &len);
--  	if (fd == -1)
--  		goto cleanup;
--  	f = fdopen(fd, "r");
--  	rewind(f);
--
--  	*mtime = local_t;
--  	/*fconflicts = repo_fetch_remote_extract_tmp(repo,
--  			repo_conflicts_archive, "txz", &local_t,
--  			&rc, repo_conflicts_file);*/
--
--  	/* Load local repository data */
--  	xasprintf(&path, "%s-pkgtemp", name);
--  	rename(name, path);
--  	pkg_register_cleanup_callback(rollback_repo, (void *)name);
--  	rc = pkg_repo_binary_init_update(repo);
--  	if (rc != EPKG_OK) {
--  		rc = EPKG_FATAL;
--  		goto cleanup;
--  	}


      Event.emit_debug (1, "Proceed: reading new packagesite.yaml for " & SQ (reponame));
      Event.emit_progress_start ("Processing entries");

      --  200MB should be enough
      declare
         db  : sqlite_h.sqlite3_Access renames repositories.Element (repo_key).sqlite_handle;
         res : Action_Result;
         onward : Boolean := True;
         intrax : Boolean := False;
      begin
         --  FreeBSD set PRAGMA page_size = getpagesize().
         --  It's unclear what the benefit is over the default 4Kb page size.
         --  Omit this PRAGMA for now
         res := CommonSQL.exec (db, "PRAGMA mmap_size = 209715200;");
         res := CommonSQL.exec (db, "PRAGMA foreign_keys = OFF;");
         res := CommonSQL.exec (db, "PRAGMA synchronous = OFF;");

         if not CommonSQL.transaction_begin (db, "REPO") then
            onward := False;
         end if;

         if onward then
            intrax := True;

         end if;
      end;

      return RESULT_FATAL;
   end update_proceed;


end Core.Repo.Operations;
