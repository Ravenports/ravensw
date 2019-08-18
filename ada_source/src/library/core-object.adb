--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Ucl;
with Interfaces.C;

package body Core.Object is

   package IC renames Interfaces.C;

   --------------------------------------------------------------------------------------------
   --  pkg_object_string
   --------------------------------------------------------------------------------------------
   function pkg_object_string (obj : access constant libucl.ucl_object_t) return String is
   begin
      return Ucl.ucl_object_tostring_forced (obj);
   end pkg_object_string;


   --------------------------------------------------------------------------------------------
   --  pkg_object_string
   --------------------------------------------------------------------------------------------
   function pkg_object_int (obj : access constant libucl.ucl_object_t) return Ucl.int64
   is
      use type libucl.ucl_type;
   begin
      if obj = null or else
        obj.c_type /= libucl.UCL_INT
      then
         return 0;
      end if;

      return Ucl.ucl_object_toint (obj);
   end pkg_object_int;


   --------------------------------------------------------------------------------------------
   --  pkg_object_bool
   --------------------------------------------------------------------------------------------
   function pkg_object_bool (obj : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
   begin
      if obj = null or else
        obj.c_type /= libucl.UCL_BOOLEAN
      then
         return False;
      end if;

      return Ucl.ucl_object_toboolean (obj);
   end pkg_object_bool;


   --------------------------------------------------------------------------------------------
   --  pkg_object_dump
   --------------------------------------------------------------------------------------------
   function pkg_object_dump (obj : access constant libucl.ucl_object_t) return String
   is
   begin
      if obj = null then
         return "";
      end if;
      return Ucl.ucl_dump (obj);
   end pkg_object_dump;

end Core.Object;
