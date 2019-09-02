--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Repo.Binary;
with Core.Pkg;

package Core.Repo_Operations is

   Ops_Binary : aliased Repo.Binary.Repo_Operations_Binary;
   Ops_Binary_Access : Repo.Repo_Ops_Access := Ops_Binary'Access;

   Ops : array (Pkg.repo_ops_variant'Range) of Repo.Repo_Ops_Access :=
     (Pkg.binary => Ops_Binary_Access);

end Core.Repo_Operations;
