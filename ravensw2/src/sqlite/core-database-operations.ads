--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Ada.Containers.Vectors;
private with Interfaces.C;

private with Core.Unix;
private with sqlite_h;

package Core.Database.Operations is

   type RDB_Connection is limited private;
   type RDB_Connection_Access is access all RDB_Connection;

   --  rdb_open with a blank reponame opens all active repositories
   --  rdb_open_all is equivalent to above
   --  rdb_open with non-blank reponame open identified repository, if active

   function rdb_open
     (db       : in out RDB_Connection;
      dbtype   : RDB_Source;
      reponame : String)
      return Action_Result;

   function rdb_open_all
     (db       : in out RDB_Connection;
      dbtype   : RDB_Source)
      return Action_Result;

   procedure rdb_close
     (db       : in out RDB_Connection);

   function rdb_obtain_lock
     (db       : in out RDB_Connection;
      locktype : RDB_Lock_Type) return Boolean;

   function rdb_release_lock
     (db       : in out RDB_Connection;
      locktype : RDB_Lock_Type) return Boolean;

   function database_access
     (mode  : RDB_Mode_Flags;
      dtype : RDB_Type)
      return Action_Result;

private

   package CON renames Ada.Containers;
   package IC  renames Interfaces.C;

   type RDB_Connection is limited
      record
         sqlite             : aliased sqlite_h.sqlite3_Access;
         prstmt_initialized : Boolean;
      end record;

   function rdb_open_remote (db       : in out RDB_Connection;
                             dbtype   : RDB_Source;
                             reponame : String)
                             return Action_Result;

   function rdb_profile_callback
     (trace_type : IC.unsigned;
      ud   : sqlite_h.Void_Ptr;
      stmt : sqlite_h.Void_Ptr;
      x    : sqlite_h.Void_Ptr) return IC.int;
   pragma Export (C, rdb_profile_callback);

   function rdb_try_lock
     (db        : in out RDB_Connection;
      lock_sql  : String;
      lock_type : RDB_Lock_Type;
      upgrade   : Boolean) return Boolean;

   function rdb_write_lock_pid (db : in out RDB_Connection) return Action_Result;

   function rdb_check_lock_pid (db : in out RDB_Connection) return Action_Result;

   function rdb_reset_lock (db : in out RDB_Connection) return Boolean;

   function rdb_remove_lock_pid
     (db  : in out RDB_Connection;
      pid : Unix.Process_ID) return Boolean;

end Core.Database.Operations;
