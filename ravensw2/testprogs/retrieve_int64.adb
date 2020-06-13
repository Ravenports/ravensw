--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Core.CommonSQL;
with sqlite_h;
with SQLite;

use Core;

procedure retrieve_int64
is
   package TIO renames Ada.Text_IO;

   res_int64 : int64;
   handle    : aliased sqlite_h.sqlite3_Access;
   okay      : Boolean;
   reponame  : constant String := "Raven";
   db_path   : constant String := "/var/db/ravensw/repo-" & reponame & ".sqlite.dev";
   intsrc    : constant String := "retrieve_int64.adb";
   sql       : constant String := "SELECT count(name) FROM sqlite_master " &
                                           "WHERE type='table' AND name='repodata'";
begin
   okay := SQLite.initialize_sqlite;
   SQLite.rdb_syscall_overload;
   if not SQLite.open_sqlite_database_readonly (db_path, handle'Access) then
      TIO.Put_Line ("failed to open " & db_path);
      if SQLite.database_corrupt (handle) then
         TIO.Put_Line ("Database is corrupt.");
      end if;
      return;
   end if;

   if CommonSQL.get_int64 (db      => handle,
                           srcfile => intsrc,
                           func    => "retrieve_int64",
                           sql     => sql,
                           res     => res_int64,
                           silence => False) /= RESULT_OK
   then
      TIO.Put_Line ("error: name count " & sql);
      SQLite.close_database (handle);
      return;
   end if;

   if res_int64 /= 1 then
      TIO.Put_Line ("Repository " & reponame &
                    " contains no repodata table, database must be recreated");
   else
      TIO.Put_Line ("Success - 1 repodata table detected.");
   end if;

   SQLite.close_database (handle);


end retrieve_int64;
