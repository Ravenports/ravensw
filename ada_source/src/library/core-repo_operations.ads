--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Repo.Binary;
with Core.Pkg;

package Core.Repo_Operations is

   Ops_Binary : aliased Repo.Binary.Repo_Operations_Binary;
   Ops_Binary_Access : Repo.Repo_Ops_Access := Ops_Binary'Access;

   ACCESS_F_OK : constant Core.Repo.mode_t := 0;
   ACCESS_X_OK : constant Core.Repo.mode_t := 1;
   ACCESS_W_OK : constant Core.Repo.mode_t := 2;
   ACCESS_R_OK : constant Core.Repo.mode_t := 4;

   Ops : array (Pkg.repo_ops_variant'Range) of Repo.Repo_Ops_Access :=
     (Pkg.binary => Ops_Binary_Access);

end Core.Repo_Operations;
