--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;

package body Core.Ucl is

   package ICS renames Interfaces.C.Strings;

   --------------------------------------------------------------------
   --  ucl_object_find_key
   --------------------------------------------------------------------
   function ucl_object_find_key (obj : pkg_object;
                                 key : String) return pkg_object
   is
      ckey   : ICS.chars_ptr;
      result : pkg_object;
   begin
      ckey := ICS.New_String (key);
      result := libucl.ucl_object_lookup (obj, ckey);
      ICS.Free (ckey);
      return result;
   end ucl_object_find_key;

end Core.Ucl;
