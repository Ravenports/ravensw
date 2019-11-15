--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.PkgDB;

package Core.Iterators.Repo is

   type Iterator_Repo is new Base_Iterators with private;

   overriding
   procedure Free  (this : in out Iterator_Repo);

   overriding
   procedure Reset (this : in out Iterator_Repo);

   overriding
   function Next
     (this    : in out Iterator_Repo;
      pkg_ptr : in out T_pkg_Access;
      flags   : Load_Flags) return Pkg_Error_Type;

   overriding
   function count (this : in out Iterator_Repo) return Integer;

   overriding
   function create_invalid_iterator return Iterator_Repo;

   function create (db_access : Core.PkgDB.struct_pkgdb_Access) return Iterator_Repo;

private

   type Iterator_Repo is new Base_Iterators with
      record
         db_access     : Core.PkgDB.struct_pkgdb_Access;
         counter       : Natural;
      end record;

end Core.Iterators.Repo;
