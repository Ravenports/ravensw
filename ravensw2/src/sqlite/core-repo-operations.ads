--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


package Core.Repo.Operations is

   function open_repository (reponame : String) return Action_Result;

   procedure close_all_open_repositories;

private

   type repository_stmt_index is
     (PKG,
      DEPS,
      CAT1,
      CAT2,
      LIC1,
      LIC2,
      OPT1,
      OPT2,
      SHLIB1,
      SHLIB_REQD,
      SHLIB_PROV,
      ANNOTATE1,
      ANNOTATE2,
      EXISTS,
      REPO_VERSION,
      DELETE,
      PROVIDE,
      PROVIDES,
      REQUIRE,
      REQUIRES
     );

   prepared_statements : array (repository_stmt_index) of sqlite_h.sqlite3_stmt_Access;

   function run_transaction (handle : sqlite_h.sqlite3_Access;
                             query  : String)
                             return Boolean;

   function transaction_begin    (handle : sqlite_h.sqlite3_Access) return Boolean;
   function transaction_commit   (handle : sqlite_h.sqlite3_Access) return Boolean;
   function transaction_rollback (handle : sqlite_h.sqlite3_Access) return Boolean;

   procedure close_repository (reponame : Text; commit : Boolean);

   function sqlite_filename (reponame : String) return String;
   function meta_filename (reponame : String) return String;

end Core.Repo.Operations;
