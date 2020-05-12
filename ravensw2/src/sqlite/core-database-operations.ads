--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Ada.Containers.Vectors;
private with Interfaces.C;
private with sqlite_h;

package Core.Database.Operations is

   type RDB_Connection is limited private;
   type RDB_Connection_Access is access all RDB_Connection;

   --  rdb_open with a blank reponame opens all active repositories
   --  rdb_open_all is equivalent to above
   --  rdb_open with non-blank reponame open identified repository, if active

   function rdb_open        (db       : in out RDB_Connection;
                             dbtype   : RDB_Source;
                             reponame : String)
                             return Action_Result;
   function rdb_open_all    (db       : in out RDB_Connection;
                             dbtype   : RDB_Source)
                             return Action_Result;
   procedure rdb_close      (db       : in out RDB_Connection);

private

   package CON renames Ada.Containers;
   package IC  renames Interfaces.C;

   function transaction_begin    (db : RDB_Connection; savepoint : String) return Boolean;
   function transaction_commit   (db : RDB_Connection; savepoint : String) return Boolean;
   function transaction_rollback (db : RDB_Connection; savepoint : String) return Boolean;

   function get_pragma (db      : sqlite_h.sqlite3_Access;
                        sql     : String;
                        res     : out int64;
                        silence : Boolean) return Action_Result;

   package Text_Jar is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");

   type RDB_Connection is limited
      record
         sqlite             : aliased sqlite_h.sqlite3_Access;
         prstmt_initialized : Boolean;
         repos              : Text_Jar.Vector;
      end record;

   procedure ERROR_SQLITE (db : sqlite_h.sqlite3_Access; func : String; query : String);

   function run_transaction (db        : sqlite_h.sqlite3_Access;
                             query     : String;
                             savepoint : String)
                             return Boolean;

   function rdb_open_remote (db       : in out RDB_Connection;
                             dbtype   : RDB_Source;
                             reponame : String)
                             return Action_Result;

   function rdb_open_repository (db       : in out RDB_Connection;
                                 reponame : String) return Action_Result;

   function rdb_profile_callback
     (trace_type : IC.unsigned;
      ud   : sqlite_h.Void_Ptr;
      stmt : sqlite_h.Void_Ptr;
      x    : sqlite_h.Void_Ptr) return IC.int;
   pragma Export (C, rdb_profile_callback);

end Core.Database.Operations;
