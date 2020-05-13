--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with sqlite_h;

package Core.CommonSQL is

   procedure ERROR_SQLITE (db : sqlite_h.sqlite3_Access; func : String; query : String);

   function transaction_begin    (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean;
   function transaction_commit   (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean;
   function transaction_rollback (db : sqlite_h.sqlite3_Access; savepoint : String) return Boolean;

   function get_pragma (db      : sqlite_h.sqlite3_Access;
                        sql     : String;
                        res     : out int64;
                        silence : Boolean) return Action_Result;

   function exec (db : sqlite_h.sqlite3_Access; sql : String) return Action_Result;

private

   function run_transaction (db        : sqlite_h.sqlite3_Access;
                             query     : String;
                             savepoint : String)
                             return Boolean;
end Core.CommonSQL;
