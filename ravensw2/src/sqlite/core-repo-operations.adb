--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Event;
with SQLite;

package body Core.Repo.Operations is

   --------------------------------------------------------------------
      --  close_repository
   --------------------------------------------------------------------
   procedure close_repository (reponame : String; commit : Boolean)
   is
      procedure close_database (key : Text; Element : in out A_repo);

      rkey : Text := SUS (reponame);

      procedure close_database (key : Text; Element : in out A_repo)
      is
         repository : A_repo renames Element;
      begin
         SQLite.close_database (repository.sqlite_handle);
         repository.sqlite_handle := null;
      end close_database;
   begin
      if repositories.Contains (rkey) then

         if not SQLite.db_connected (repositories.Element (rkey).sqlite_handle) then
            return;
         end if;

         if commit then
            if not transaction_commit (repositories.Element (rkey).sqlite_handle) then
               Event.emit_error ("close_repository(): Failed to commit transaction");
               return;
            end if;
         end if;

         for S in repository_stmt_index loop
            SQLite.finalize_statement (prepared_statements (S));
         end loop;

         repositories.Update_Element (Position => repositories.Find (rkey),
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

end Core.Repo.Operations;
