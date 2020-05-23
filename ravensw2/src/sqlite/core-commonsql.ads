--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with sqlite_h;

package Core.CommonSQL is

   procedure ERROR_SQLITE (db      : sqlite_h.sqlite3_Access;
                           srcfile : String;
                           func    : String;
                           query   : String);

   function transaction_begin    (db        : sqlite_h.sqlite3_Access;
                                  srcfile   : String;
                                  func      : String;
                                  savepoint : String) return Boolean;
   function transaction_commit   (db        : sqlite_h.sqlite3_Access;
                                  srcfile   : String;
                                  func      : String;
                                  savepoint : String) return Boolean;
   function transaction_rollback (db        : sqlite_h.sqlite3_Access;
                                  srcfile   : String;
                                  func      : String;
                                  savepoint : String) return Boolean;

   function get_pragma (db      : sqlite_h.sqlite3_Access;
                        srcfile : String;
                        func    : String;
                        sql     : String;
                        res     : out int64;
                        silence : Boolean) return Action_Result;

   function exec (db : sqlite_h.sqlite3_Access; sql : String) return Action_Result;

   procedure create_function (db    : sqlite_h.sqlite3_Access;
                              name  : String;
                              nargs : Natural;
                              cb    : sqlite_h.cb_xFuncStep);

private

   function run_transaction (db        : sqlite_h.sqlite3_Access;
                             srcfile   : String;
                             func      : String;
                             query     : String;
                             savepoint : String)
                             return Boolean;
end Core.CommonSQL;
