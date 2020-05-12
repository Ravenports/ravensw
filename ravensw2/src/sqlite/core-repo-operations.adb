--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Event;
with Core.Context;
with Core.Unix;
with SQLite;

package body Core.Repo.Operations is

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
         SQLite.close_database (repository.sqlite_handle);
         repository.sqlite_handle := null;
      end close_database;
   begin
      if repositories.Contains (reponame) then

         if not SQLite.db_connected (repositories.Element (reponame).sqlite_handle) then
            return;
         end if;

         if commit then
            if not transaction_commit (repositories.Element (reponame).sqlite_handle) then
               Event.emit_error ("close_repository(): Failed to commit transaction");
               return;
            end if;
         end if;

         for S in repository_stmt_index loop
            SQLite.finalize_statement (prepared_statements (S));
         end loop;

         repositories.Update_Element (Position => repositories.Find (reponame),
                                      Process  => close_database'Access);
      else
         raise invalid_repo_name;
      end if;
   end close_repository;


      --------------------------------------------------------------------
   --  run_transaction
   --------------------------------------------------------------------
   function run_transaction (handle : sqlite_h.sqlite3_Access; query : String)  return Boolean
   is
      stmt : aliased sqlite_h.sqlite3_stmt_Access;
      msg  : constant String := "sqlite error while executing " & query &
        " in file core-repo-operations.adb, run_transaction(): ";
   begin
      Event.emit_debug (4, "RDB: running '" & query & "'");
      if SQLite.prepare_sql (handle, query, stmt'Access) then
         if not SQLite.step_through_statement (stmt => stmt, num_retries => 6) then
            Event.emit_error (msg & SQLite.get_last_error_message (handle));
            SQLite.finalize_statement (stmt);
            return False;
         end if;
         SQLite.finalize_statement (stmt);
         return True;
      else
         Event.emit_error (msg & SQLite.get_last_error_message (handle));
         return False;
      end if;
   end run_transaction;


   --------------------------------------------------------------------
   --  transaction_begin
   --------------------------------------------------------------------
   function transaction_begin (handle : sqlite_h.sqlite3_Access) return Boolean is
   begin
      return run_transaction (handle, "BEGIN IMMEDIATE TRANSACTION");
   end transaction_begin;


   --------------------------------------------------------------------
   --  transaction_commit
   --------------------------------------------------------------------
   function transaction_commit (handle : sqlite_h.sqlite3_Access) return Boolean is
   begin
      return run_transaction (handle, "COMMIT TRANSACTION");
   end transaction_commit;


   --------------------------------------------------------------------
   --  transaction_rollback
   --------------------------------------------------------------------
   function transaction_rollback (handle : sqlite_h.sqlite3_Access) return Boolean is
   begin
      return run_transaction (handle, "ROLLBACK TRANSACTION");
   end transaction_rollback;


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
   function open_repository (reponame : String) return Action_Result
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
--            tmp := Repo_Meta.pkg_repo_meta_load (fd, success);
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
      end open_database;

   begin
      dbdirfd := Context.reveal_db_directory_fd;
      repositories.Update_Element (repositories.Find (SUS (reponame)), open_database'Access);

      return result;
   end open_repository;

end Core.Repo.Operations;
