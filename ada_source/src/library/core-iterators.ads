--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg; use Core.Pkg;

package Core.Iterators is

   type Base_Iterators is abstract tagged private;
   type Iterators_Access is access all Base_Iterators'Class;

   PKG_LOAD_FLAG_BASIC            : constant Load_Flags := 0;
   PKG_LOAD_FLAG_DEPS             : constant Load_Flags := 2 ** 0;
   PKG_LOAD_FLAG_RDEPS            : constant Load_Flags := 2 ** 1;
   PKG_LOAD_FLAG_FILES            : constant Load_Flags := 2 ** 2;
   PKG_LOAD_FLAG_SCRIPTS          : constant Load_Flags := 2 ** 3;
   PKG_LOAD_FLAG_OPTIONS          : constant Load_Flags := 2 ** 4;
   PKG_LOAD_FLAG_DIRS             : constant Load_Flags := 2 ** 5;
   PKG_LOAD_FLAG_CATEGORIES       : constant Load_Flags := 2 ** 6;
   PKG_LOAD_FLAG_LICENSES         : constant Load_Flags := 2 ** 7;
   PKG_LOAD_FLAG_USERS            : constant Load_Flags := 2 ** 8;
   PKG_LOAD_FLAG_GROUPS           : constant Load_Flags := 2 ** 9;
   PKG_LOAD_FLAG_SHLIBS_REQUIRED  : constant Load_Flags := 2 ** 10;
   PKG_LOAD_FLAG_SHLIBS_PROVIDED  : constant Load_Flags := 2 ** 11;
   PKG_LOAD_FLAG_ANNOTATIONS      : constant Load_Flags := 2 ** 12;
   PKG_LOAD_FLAG_CONFLICTS        : constant Load_Flags := 2 ** 13;
   PKG_LOAD_FLAG_PROVIDES         : constant Load_Flags := 2 ** 14;
   PKG_LOAD_FLAG_REQUIRES         : constant Load_Flags := 2 ** 15;
   PKG_LOAD_FLAG_CONFIG_FILES     : constant Load_Flags := 2 ** 16;
   PKG_LOAD_FLAG_DEP_FORMULA      : constant Load_Flags := 2 ** 17;

   type Iterator_Flags is mod 2 ** 2;

   PKGDB_IT_FLAG_CYCLED      : constant Iterator_Flags := 2 ** 0;
   PKGDB_IT_FLAG_ONCE        : constant Iterator_Flags := 2 ** 1;
   PKGDB_IT_FLAG_AUTO        : constant Iterator_Flags := 2 ** 2;

   procedure Free  (this : in out Base_Iterators) is abstract;
   procedure Reset (this : in out Base_Iterators) is abstract;
   function Next   (this : in out Base_Iterators;
                    pkg_ptr : in out T_pkg_Access;
                    flags : Load_Flags) return Pkg_Error_Type is abstract;

   function create_invalid_iterator return Base_Iterators is abstract;

   function count (this : in out Base_Iterators) return Integer is abstract;

   function invalid_iterator (this : Base_Iterators) return Boolean;

private

   type Base_Iterators is abstract tagged
      record
         valid : Boolean;
      end record;

end Core.Iterators;
