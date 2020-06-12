--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Core.CommonSQL;
with Core.VFS;
with sqlite_h;
with SQLite;

use Core;

procedure check_bind
is
   package TIO renames Ada.Text_IO;

   res_int64 : int64;
   handle    : aliased sqlite_h.sqlite3_Access;
   okay      : Boolean;
   reponame  : constant String := "Raven";
   db_path   : constant String := "/var/db/ravensw/repo-" & reponame & ".sqlite.dev";
   intsrc    : constant String := "retrieve_int64.adb";
   sql       : constant String := "SELECT id, origin, version, comment from packages where name = ?1";
   stmt      : aliased sqlite_h.sqlite3_stmt_Access;
begin
   okay := SQLite.initialize_sqlite;
   if not SQLite.open_sqlite_database_readonly (db_path, handle'Access) then
      TIO.Put_Line ("failed to open " & db_path);
      if SQLite.database_corrupt (handle) then
         TIO.Put_Line ("Database is corrupt.");
      end if;
      return;
   end if;

   if not SQLite.prepare_sql (handle, sql, stmt'Access) then
      TIO.Put_Line ("error preparing: " & sql);
      return;
   end if;

   SQLite.bind_string (stmt, 1, "AdaBrowse-complete-standard");

   loop
      case sqlite_h.sqlite3_step (stmt) is
         when sqlite_h.SQLITE_ROW =>
            TIO.Put_Line ("id      ="  & SQLite.retrieve_integer (stmt, 0)'Img);
            TIO.Put_Line ("origin  = " & SQLite.retrieve_string (stmt, 1));
            TIO.Put_Line ("version = " & SQLite.retrieve_string (stmt, 2));
            TIO.Put_Line ("comment = " & SQLite.retrieve_string (stmt, 3));
         when sqlite_h.SQLITE_DONE =>
            TIO.Put_Line ("finished");
            exit;
         when others =>
            TIO.Put_Line ("something else");
      end case;
   end loop;

   SQLite.finalize_statement (stmt);
   SQLite.close_database (handle);

end check_bind;
