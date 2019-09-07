--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core;

private with Interfaces.C;
private with sqlite_h;

package Core.Repo.Binary_Init is

   function pkg_repo_binary_init (reponame : Text) return Boolean;

private
   package IC renames Interfaces.C;

   procedure sqlite_file_exists
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access);
   pragma Convention (C, sqlite_file_exists);

end Core.Repo.Binary_Init;
