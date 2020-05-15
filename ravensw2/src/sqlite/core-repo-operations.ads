--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Core.Repo.Operations is

   function open_repository (reponame : String; readonly : Boolean) return Action_Result;

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

   prepared_statements : array (repository_stmt_index) of aliased sqlite_h.sqlite3_stmt_Access;

   --  Close open repository database
   procedure close_repository (reponame : Text; commit : Boolean);

   function sqlite_filename (reponame : String) return String;
   function meta_filename (reponame : String) return String;

   --  prepare repository database for use
   function initialize_repository (reponame : Text) return Action_Result;

   --  create repo database file and populate it
   function create_repository (reponame : String) return Action_Result;

end Core.Repo.Operations;
