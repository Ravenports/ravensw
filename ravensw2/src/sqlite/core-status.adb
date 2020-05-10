--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Directories;
with GNAT.OS_Lib;
with Core.Config;
with SQLite;
with sqlite_h;

package body Core.Status is

   package DIR renames Ada.Directories;
   package OSL renames GNAT.OS_Lib;

   --------------------------------------------------------------------
   --  ravensw_status
   --------------------------------------------------------------------
   function ravensw_status return Activation_Status_Output
   is
      result : Activation_Status_Output;
      dbdir  : constant String := Config.configuration_value (Config.dbdir);
      dbfile : constant String := dbdir & "/local.sqlite";
   begin
      result.count := 0;

      --  Does the local.sqlite pkg database exist, and can we open it for reading?

      if not OSL.Is_Readable_File (dbfile) then
         result.status := ACT_STATUS_NODB;
         return result;
      end if;

      --  Try opening the DB and preparing and running a simple query.
      declare
         dbsuccess : Boolean := False;
         sql       : constant String := "SELECT COUNT(*) FROM packages";
         db        : aliased sqlite_h.sqlite3_Access;
         stmt      : aliased sqlite_h.sqlite3_stmt_Access;
      begin
         if SQLite.initialize_sqlite then
            if SQLite.open_sqlite_database_readonly (dbfile, db'Access) then
               if SQLite.prepare_sql (db, sql, stmt'Access) then
                  if SQLite.step_through_statement (stmt) then
                     result.count := Integer (SQLite.retrieve_integer (stmt, 0));
                     dbsuccess := True;
                  end if;
                  SQLite.finalize_statement (stmt);
               end if;
               SQLite.close_database (db);
            end if;
            SQLite.shutdown_sqlite;
         end if;

         if not dbsuccess then
            result.status := ACT_STATUS_BAD_DB;
            return result;
         end if;
      end;

      if result.count > 0 then
         result.status := ACT_STATUS_ACTIVE;
      else
         result.status := ACT_STATUS_NOPACKAGES;
      end if;

      return result;
   end ravensw_status;

end Core.Status;
