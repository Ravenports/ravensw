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
with Core.Utilities;
with Core.Manifest;
with Core.Version;
with SQLite;


package body Core.Repo.Operations is

   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package ROS renames Core.Repo.Operations.Schema;

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
         ROS.repo_prstmt_finalize (repository.sqlite_handle);
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

         --  initialize repository database
         if initialize_repository (key) /= RESULT_OK then
            Event.emit_error ("Unable to initialize open " & reponame & " repository database.");
            SQLite.close_database (repository.sqlite_handle);
            return;
         end if;

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
            upres := ROS.repo_upgrade (repository.sqlite_handle, reponame);
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
            if ROS.repo_prstmt_initialize (db) /= RESULT_OK then
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
         if ROS.import_schema_2013 (db) /= RESULT_OK then
            SQLite.close_database (db);
            return;
         end if;
         if ROS.repo_upgrade (db, reponame) /= RESULT_OK then
            SQLite.close_database (db);
            return;
         end if;
         declare
            stmt : SQLite.thick_stmt;
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
            if not SQLite.prepare_sql (db, sql2, stmt) then
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
      local_time : aliased Unix.T_epochtime;
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
                                timestamp => local_time'Access) = RESULT_FATAL
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
              filename  => USS (my_repo.meta.manifests_archive),
              innerfile => USS (my_repo.meta.manifests),
              timestamp => local_time'Access,
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
            if DIR.Exists (filepath) then
               DIR.Copy_File (filepath, backup);
            end if;
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
            if DIR.Exists (filepath) then
               DIR.Delete_File (filepath);
            end if;
            if DIR.Exists (backup) then
               DIR.Rename (backup, filepath);
            end if;
         else
            --  remove temporary backup
            if DIR.Exists (backup) then
               DIR.Delete_File (backup);
            end if;
         end if;
         if DIR.Exists (tmp_manifest) then
            DIR.Delete_File (tmp_manifest);
         end if;

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
      SQLite.rdb_syscall_overload;
      if not Repo.repository_is_active (reponame) then
         return RESULT_OK;
      end if;

      Event.emit_debug (1, "rdb: verifying update for " & reponame);

      --  First of all, try to open and init repo and check whether it is fine
      if open_repository (reponame, False) /= RESULT_OK then
         Event.emit_debug (1, "rdb: need forced update of " & reponame);
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
   --  push_arg (number)
   --------------------------------------------------------------------
   procedure push_arg (args : in out Set_Repo_Stmt_Args.Vector; numeric_arg : int64)
   is
      new_entry : Repo_Stmt_Argument;
   begin
      new_entry.datatype := Provide_Number;
      new_entry.data_number := numeric_arg;
      args.Append (new_entry);
   end push_arg;


   --------------------------------------------------------------------
   --  push_arg (string)
   --------------------------------------------------------------------
   procedure push_arg (args : in out Set_Repo_Stmt_Args.Vector; textual_arg : String)
   is
      new_entry : Repo_Stmt_Argument;
   begin
      new_entry.datatype := Provide_String;
      new_entry.data_string := SUS (textual_arg);
      args.Append (new_entry);
   end push_arg;


   --------------------------------------------------------------------
   --  push_arg (unbounded string)
   --------------------------------------------------------------------
   procedure push_arg (args : in out Set_Repo_Stmt_Args.Vector; textual_arg : Text)
   is
      new_entry : Repo_Stmt_Argument;
   begin
      new_entry.datatype := Provide_String;
      new_entry.data_string := textual_arg;
      args.Append (new_entry);
   end push_arg;


   --------------------------------------------------------------------
   --  push_arg (boolean)
   --------------------------------------------------------------------
   procedure push_arg (args : in out Set_Repo_Stmt_Args.Vector; boolean_arg : Boolean)
   is
      new_entry : Repo_Stmt_Argument;
   begin
      new_entry.datatype := Provide_Number;
      if boolean_arg then
         new_entry.data_number := 1;
      else
         new_entry.data_number := 0;
      end if;
      args.Append (new_entry);
   end push_arg;


   --------------------------------------------------------------------
   --  delete_conflicting_package
   --------------------------------------------------------------------
   function delete_conflicting_package
     (origin   : Text;
      version  : Text;
      pkg_path : Text;
      forced   : Boolean) return Action_Result
   is
      osversion : constant String := ROS.retrieve_prepared_version (origin);
   begin
      if IsBlank (osversion) then
         return RESULT_FATAL;
      end if;
      if forced then
         return ROS.kill_package (origin);
      else
         case Core.Version.pkg_version_cmp (osversion, USS (version)) is
            when -1 =>
               Event.emit_error
                 ("duplicate package origin: replacing older version " & osversion
                  & " in repo with package " & USS (pkg_path) & " for origin " & USS (origin));
               return ROS.kill_package (origin);
            when 0 | 1 =>
               Event.emit_error
                 ("duplicate package origin: package " & USS (pkg_path)
                  & " is not newer than version " & osversion
                  & " already in repo for origin " & USS (origin));
               --  keep what is already in the repo
               return RESULT_END;
         end case;
      end if;
   end delete_conflicting_package;


   --------------------------------------------------------------------
   --  add_from_manifest
   --------------------------------------------------------------------
   function add_from_manifest
     (my_repo  : A_repo;
      manifest : String) return Action_Result
   is
      my_pkg : aliased Pkgtypes.A_Package;
      abi    : Text;
   begin
      my_pkg.package_type := Pkgtypes.PKG_REMOTE;
      if Core.Manifest.parse_manifest (my_pkg'Unchecked_Access, manifest) /= RESULT_OK then
         return RESULT_FATAL;
      end if;

      if IsBlank (my_pkg.digest) or else not Checksum.checksum_is_valid (my_pkg.digest) then
         if Checksum.checksum_calculate (my_pkg'Unchecked_Access, null) /= RESULT_OK then
            return RESULT_FATAL;
         end if;
      end if;

      --  Does ravensw ever run into a blank abi situation?  Might be a pkgng thing ...
      if IsBlank (my_pkg.abi) then
         abi := my_pkg.arch;
      else
         abi := my_pkg.abi;
      end if;


      if IsBlank (abi) or else not Utilities.is_valid_abi (USS (abi), True) then
         Event.emit_error ("repository " & Repo.repo_name (my_repo) &
                             " contains packages with wrong ABI: " & USS (abi));
         return RESULT_FATAL;
      end if;
      my_pkg.reponame := SUS (Repo.repo_name (my_repo));

      return add_package_to_repository (pkg_access => my_pkg'Unchecked_Access,
                                        my_repo    => my_repo,
                                        pkg_path   => "",
                                        forced     => True);
   end add_from_manifest;


   --------------------------------------------------------------------
   --  add_package_to_repository
   --------------------------------------------------------------------
   function add_package_to_repository
     (pkg_access : Pkgtypes.A_Package_Access;
      my_repo    : A_repo;
      pkg_path   : String;
      forced     : Boolean) return Action_Result
   is
      function get_arch return String;
      function insert_main_package (rc : out Action_Result) return Boolean;
      procedure spit_out_error     (index : ROS.repository_stmt_index; extra : String := "");
      procedure insert_dependency  (position : Pkgtypes.Dependency_Crate.Cursor);
      procedure insert_category    (position : Pkgtypes.Text_Crate.Cursor);
      procedure insert_license     (position : Pkgtypes.Text_Crate.Cursor);
      procedure insert_shlib_reqd  (position : Pkgtypes.Text_Crate.Cursor);
      procedure insert_shlib_prov  (position : Pkgtypes.Text_Crate.Cursor);
      procedure insert_provide     (position : Pkgtypes.Text_Crate.Cursor);
      procedure insert_require     (position : Pkgtypes.Text_Crate.Cursor);
      procedure insert_option      (position : Pkgtypes.Package_NVPairs.Cursor);
      procedure insert_annotations (position : Pkgtypes.Package_NVPairs.Cursor);
      procedure insert_generic     (index1, index2 : ROS.repository_stmt_index; value : Text);
      procedure insert_generic_nv  (index1, index2 : ROS.repository_stmt_index;
                                    name, value : Text);

      rc      : Action_Result;
      problem : Boolean := False;

      function get_arch return String is
      begin
         if IsBlank (pkg_access.abi) then
            return USS (pkg_access.arch);
         else
            return USS (pkg_access.abi);
         end if;
      end get_arch;

      procedure spit_out_error (index : ROS.repository_stmt_index; extra : String := "") is
      begin
         CommonSQL.ERROR_SQLITE
           (my_repo.sqlite_handle,
            internal_srcfile,
            "add_package_to_repository",
            "Prep stmt " & index'Img & extra);
      end spit_out_error;

      function insert_main_package (rc : out Action_Result) return Boolean
      is
         args : Set_Repo_Stmt_Args.Vector;
         index : constant ROS.repository_stmt_index := ROS.PKG;
         sqerr : sqlite_h.enum_error_types;
      begin
         --  "TTTT_TTTT_TIII_TTTT_I",
         push_arg (args, pkg_access.origin);
         push_arg (args, pkg_access.name);
         push_arg (args, pkg_access.version);
         push_arg (args, pkg_access.comment);

         push_arg (args, pkg_access.desc);
         push_arg (args, get_arch);
         push_arg (args, pkg_access.maintainer);
         push_arg (args, pkg_access.www);

         push_arg (args, pkg_access.prefix);
         push_arg (args, int64 (pkg_access.pkgsize));
         push_arg (args, int64 (pkg_access.flatsize));
         push_arg (args, int64 (Pkgtypes.License_Logic'Pos (pkg_access.licenselogic)));

         push_arg (args, pkg_access.sum);
         push_arg (args, pkg_access.repopath);
         push_arg (args, pkg_access.digest);
         push_arg (args, pkg_access.old_version);

         push_arg (args, pkg_access.vital);

         if ROS.run_repo_prepared_statement (index, args) then
            rc := RESULT_OK;
            return True;
         else
            sqerr := SQLite.get_last_error_code (my_repo.sqlite_handle);
            case sqerr is
               when sqlite_h.SQLITE_CONSTRAINT =>
                  Event.emit_debug (3, "Deleting conflicting package " & USS (pkg_access.origin)
                                    & "-" & USS (pkg_access.version));
                  rc := delete_conflicting_package (origin   => pkg_access.origin,
                                                    version  => pkg_access.version,
                                                    pkg_path => SUS (pkg_path),
                                                    forced   => forced);
                  case rc is
                     when RESULT_FATAL =>
                        spit_out_error (index, " (delete conflict failed)");
                     when RESULT_END =>
                        --  repo already has newer
                        null;
                     when others =>
                        --  conflict cleared, try again
                        null;
                  end case;
               when others =>
                  spit_out_error (index);
                  rc := RESULT_FATAL;
            end case;
            return False;
         end if;
      end insert_main_package;

      procedure insert_dependency (position : Pkgtypes.Dependency_Crate.Cursor)
      is
         dep : Pkgtypes.Package_Dependency renames Pkgtypes.Dependency_Crate.Element (position);
         args : Set_Repo_Stmt_Args.Vector;
         index : constant ROS.repository_stmt_index := ROS.DEPS;
      begin
         if not problem then
            push_arg (args, pkg_access.origin);
            push_arg (args, pkg_access.name);
            push_arg (args, pkg_access.version);
            push_arg (args, int64 (pkg_access.id));
            problem := ROS.run_repo_prepared_statement (index, args);
         end if;
      end insert_dependency;

      procedure insert_generic (index1, index2 : ROS.repository_stmt_index; value : Text)
      is
         args1 : Set_Repo_Stmt_Args.Vector;
         args2 : Set_Repo_Stmt_Args.Vector;
      begin
         if not problem then
            push_arg (args1, value);
            problem := ROS.run_repo_prepared_statement (index1, args1);
            if problem then
               spit_out_error (index1);
            else
               push_arg (args2, int64 (pkg_access.id));
               push_arg (args2, value);
               problem := ROS.run_repo_prepared_statement (index2, args2);
               if problem then
                  spit_out_error (index2);
               end if;
            end if;
         end if;
      end insert_generic;

      procedure insert_category (position : Pkgtypes.Text_Crate.Cursor) is
      begin
         insert_generic (ROS.CAT1,
                         ROS.CAT2,
                         Pkgtypes.Text_Crate.Element (position));
      end insert_category;

      procedure insert_license (position : Pkgtypes.Text_Crate.Cursor) is
      begin
         insert_generic (ROS.LIC1,
                         ROS.LIC2,
                         Pkgtypes.Text_Crate.Element (position));
      end insert_license;

      procedure insert_shlib_reqd (position : Pkgtypes.Text_Crate.Cursor) is
      begin
         insert_generic (ROS.SHLIB1,
                         ROS.SHLIB_REQD,
                         Pkgtypes.Text_Crate.Element (position));
      end insert_shlib_reqd;

      procedure insert_shlib_prov (position : Pkgtypes.Text_Crate.Cursor) is
      begin
         insert_generic (ROS.SHLIB1,
                         ROS.SHLIB_PROV,
                         Pkgtypes.Text_Crate.Element (position));
      end insert_shlib_prov;

      procedure insert_provide (position : Pkgtypes.Text_Crate.Cursor) is
      begin
         insert_generic (ROS.PROVIDE,
                         ROS.PROVIDES,
                         Pkgtypes.Text_Crate.Element (position));
      end insert_provide;

      procedure insert_require (position : Pkgtypes.Text_Crate.Cursor) is
      begin
         insert_generic (ROS.REQUIRE,
                         ROS.REQUIRES,
                         Pkgtypes.Text_Crate.Element (position));
      end insert_require;

      procedure insert_generic_nv (index1, index2 : ROS.repository_stmt_index; name, value : Text)
      is
         args1 : Set_Repo_Stmt_Args.Vector;
         args2 : Set_Repo_Stmt_Args.Vector;
      begin
         if not problem then
            push_arg (args1, name);
            problem := ROS.run_repo_prepared_statement (index1, args1);
            if problem then
               spit_out_error (index1);
            else
               push_arg (args2, int64 (pkg_access.id));
               push_arg (args2, name);
               push_arg (args2, value);
               problem := ROS.run_repo_prepared_statement (index2, args2);
               if problem then
                  spit_out_error (index2);
               end if;
            end if;
         end if;
      end insert_generic_nv;

      procedure insert_option (position : Pkgtypes.Package_NVPairs.Cursor) is
      begin
         insert_generic_nv (index1 => ROS.OPT1,
                            index2 => ROS.OPT2,
                            name   => Pkgtypes.Package_NVPairs.Key (position),
                            value  => Pkgtypes.Package_NVPairs.Element (position));
      end insert_option;

      procedure insert_annotations (position : Pkgtypes.Package_NVPairs.Cursor) is
      begin
         insert_generic_nv (index1 => ROS.ANNOTATE1,
                            index2 => ROS.ANNOTATE2,
                            name   => Pkgtypes.Package_NVPairs.Key (position),
                            value  => Pkgtypes.Package_NVPairs.Element (position));
      end insert_annotations;

   begin
      loop
         rc := RESULT_FATAL;
         exit when insert_main_package (rc);
         case rc is
            when RESULT_FATAL => return RESULT_FATAL;
            when RESULT_END   => return RESULT_END;
            when others       => null;
         end case;
      end loop;
      pkg_access.id :=
        Pkgtypes.Package_ID (sqlite_h.sqlite3_last_insert_rowid (my_repo.sqlite_handle));

      pkg_access.depends.Iterate (insert_dependency'Access);
      pkg_access.categories.Iterate (insert_category'Access);
      pkg_access.licenses.Iterate (insert_license'Access);
      pkg_access.options.Iterate (insert_option'Access);
      pkg_access.shlibs_reqd.Iterate (insert_shlib_reqd'Access);
      pkg_access.shlibs_prov.Iterate (insert_shlib_prov'Access);
      pkg_access.requires.Iterate (insert_require'Access);
      pkg_access.provides.Iterate (insert_provide'Access);
      pkg_access.annotations.Iterate (insert_annotations'Access);

      if problem then
         return RESULT_FATAL;
      else
         return RESULT_OK;
      end if;
   end add_package_to_repository;

end Core.Repo.Operations;
