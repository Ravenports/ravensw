--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Pkgtypes;
with Core.Database;
private with Core.Unix;

package Core.Repo.Operations is

   package Set_Repo_Stmt_Args is new CON.Vectors
     (Element_Type => Repo_Stmt_Argument,
      Index_Type   => Natural);

   function open_repository (reponame : String; readonly : Boolean) return Action_Result;

   procedure close_all_open_repositories;

   function check_repository_access
     (reponame : String;
      mode     : Database.RDB_Mode_Flags) return Action_Result;

   function update_repository (reponame : String; force : Boolean) return Action_Result;

   function delete_conflicting_package
     (origin   : Text;
      version  : Text;
      pkg_path : Text;
      forced   : Boolean) return Action_Result;

private

   internal_srcfile : constant String := "core-repo-operations.adb";

   --  Close open repository database
   procedure close_repository (reponame : Text; commit : Boolean);

   --  Repository specific database file name
   function repo_database_file (reponame : String) return String;

   --  prepare repository database for use
   function initialize_repository (reponame : Text) return Action_Result;

   --  create repo database file and populate it
   function create_repository (reponame : String) return Action_Result;

   --  subroutine of repository_update
   function update_init (reponame : String) return Action_Result;

   --  subroutine of repository_update
   function update_proceed
     (reponame : String;
      filepath : String;
      mtime    : in out Unix.T_epochtime;
      force    : Boolean) return Action_Result;

   function add_from_manifest
     (my_repo  : A_repo;
      manifest : String) return Action_Result;

   --  build up prepared statement arguments
   procedure push_arg (args : in out Set_Repo_Stmt_Args.Vector; numeric_arg : int64);
   procedure push_arg (args : in out Set_Repo_Stmt_Args.Vector; textual_arg : String);
   procedure push_arg (args : in out Set_Repo_Stmt_Args.Vector; textual_arg : Text);
   procedure push_arg (args : in out Set_Repo_Stmt_Args.Vector; boolean_arg : Boolean);

   function add_package_to_repository
     (pkg_access : Pkgtypes.A_Package_Access;
      my_repo    : A_repo;
      pkg_path   : String;
      forced     : Boolean) return Action_Result;

end Core.Repo.Operations;
