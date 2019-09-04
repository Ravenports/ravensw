--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers.Vectors;

package Core.Deps is

   package CON renames Ada.Containers;

   type pkg_dep_version_op is
     (VERSION_ANY,
      VERSION_EQ,
      VERSION_GE,
      VERSION_LE,
      VERSION_LT,
      VERSION_GT,
      VERSION_NOT);

   type pkg_dep_flag is mod 2 ** 3;

   PKG_DEP_FLAG_NORMAL  : constant pkg_dep_flag := 0;
   PKG_DEP_FLAG_REQUIRE : constant pkg_dep_flag := 2 ** 0;
   PKG_DEP_FLAG_GLOB    : constant pkg_dep_flag := 2 ** 1;
   PKG_DEP_FLAG_REGEXP  : constant pkg_dep_flag := 2 ** 2;

   type pkg_dep_version_item is
      record
         version : Text;
         op      : pkg_dep_version_op;
      end record;

   type pkg_dep_option_item is
      record
         option : Text;
         active : Boolean;
      end record;

   package pkg_dep_version_item_crate is new CON.Vectors
     (Element_Type => pkg_dep_version_item,
      Index_Type   => Natural);

   package pkg_dep_option_item_crate is new CON.Vectors
     (Element_Type => pkg_dep_option_item,
      Index_Type   => Natural);

   type pkg_dep_formula_item is
      record
         name     : Text;
         flags    : pkg_dep_flag;
         versions : pkg_dep_version_item_crate.Vector;
         options  : pkg_dep_option_item_crate.Vector;
      end record;
   type pkg_dep_formula_item_Access is access all pkg_dep_formula_item;

   package pkg_dep_formula is new CON.Vectors
     (Element_Type => pkg_dep_formula_item,
      Index_Type   => Natural);

   type pkg_formula is record
      items : pkg_dep_formula.Vector;
   end record;
   type pkg_formula_Access is access all pkg_formula;

   package formula_crate is new CON.Vectors
     (Element_Type => pkg_formula,
      Index_Type   => Natural);

   function pkg_deps_string_toop (instr : String) return pkg_dep_version_op;

   function pkg_deps_parse_formula (instr : String) return formula_crate.Vector;

end Core.Deps;
