--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

--  with Ada.Containers.Vectors;

package Core.Depends is

   type Dep_Version_Operator is
     (VERSION_ANY,
      VERSION_EQ,
      VERSION_GE,
      VERSION_LE,
      VERSION_LT,
      VERSION_GT,
      VERSION_NOT);

   type Dep_Flag is mod 2 ** 3;

   DEP_FLAG_NORMAL  : constant Dep_Flag := 0;
   DEP_FLAG_REQUIRE : constant Dep_Flag := 2 ** 0;
   DEP_FLAG_GLOB    : constant Dep_Flag := 2 ** 1;
   DEP_FLAG_REGEXP  : constant Dep_Flag := 2 ** 2;

   type Dep_Version_Item is
      record
         version : Text;
         op      : Dep_Version_Operator;
      end record;

   type Dep_Option_Item is
      record
         option : Text;
         active : Boolean;
      end record;

   function string_to_operator (instr : String) return Dep_Version_Operator;

end Core.Depends;
