--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Containers.Vectors;
private with Interfaces.C;

with Core.Pkgtypes;
private with Core.Unix;
private with sqlite_h;

package Core.Database.Operations is

   package CON renames Ada.Containers;

   package Set_Stmt_Args is new CON.Vectors
     (Element_Type => Stmt_Argument,
      Index_Type   => Natural);

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

   function rdb_connected (db : RDB_Connection_Access) return Boolean;

   function set_pkg_digest
     (pkg_access : Pkgtypes.A_Package_Access;
      rdb_access : RDB_Connection_Access) return Action_Result;

   function add_pkg_to_database
     (pkg_access : Pkgtypes.A_Package_Access;
      rdb_access : RDB_Connection_Access;
      pkg_path   : String;
      forced     : Boolean) return Action_Result;

private

   package IC  renames Interfaces.C;

   internal_srcfile : constant String := "core-database-operations.adb";

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

   function establish_connection (db : in out RDB_Connection) return Action_Result;

   --  build up prepared statement arguments
   procedure push_arg (args : in out Set_Stmt_Args.Vector; numeric_arg : int64);
   procedure push_arg (args : in out Set_Stmt_Args.Vector; textual_arg : String);
   procedure push_arg (args : in out Set_Stmt_Args.Vector; textual_arg : Text);
   procedure push_arg (args : in out Set_Stmt_Args.Vector; boolean_arg : Boolean);

end Core.Database.Operations;
