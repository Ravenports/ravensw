--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Iterators.Binary_sqlite;

package body Core.Iterators.Repo is

   package IBS renames Core.Iterators.Binary_sqlite;

   --------------------------------------------------------------------
   --  create
   --------------------------------------------------------------------
   function create (db_access : Core.PkgDB.struct_pkgdb_Access) return Iterator_Repo
   is
      product : Iterator_Repo;
   begin
      product.db_access := db_access;
      product.iterator_type := PKGDB_IT_REPO;
      product.valid := True;
      product.local := IBS.create_invalid_iterator;

      return product;
   end create;


   --------------------------------------------------------------------
   --  create_invalid_iterator
   --------------------------------------------------------------------
   overriding
   function create_invalid_iterator return Iterator_Repo
      is
      product : Iterator_Repo;
   begin
      product.db_access := null;
      product.package_type := PKG_REMOTE;
      product.valid := False;
      product.local := IBS.create_invalid_iterator;

      return product;
   end create_invalid_iterator;


   --------------------------------------------------------------------
   --  Free
   --------------------------------------------------------------------
   overriding
   procedure Free  (this : in out Iterator_Repo) is
   begin
      if this.valid then
         case this.iterator_type is
            when PKGDB_IT_REPO =>
            when PKGDB_IT_LOCAL =>

         end case;
      end if;
   end Free;


   --------------------------------------------------------------------
   --  Reset
   --------------------------------------------------------------------
   overriding
   procedure Reset (this : in out Iterator_Repo) is
   begin
      this.counter := 0;
      if this.valid then
         case this.package_type is
            when
         end case;
      end if;
   end Reset;

   overriding
   function Next (this    : in out Iterator_Repo;
                  pkg_ptr : in out T_pkg_Access;
                  flags   : Load_Flags) return Pkg_Error_Type;


end Core.Iterators.Repo;
