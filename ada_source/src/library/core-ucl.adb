--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package body Core.Ucl is

   package ICS renames Interfaces.C.Strings;
   package ICX renames Interfaces.C.Extensions;

   --------------------------------------------------------------------
   --  ucl_object_find_key
   --------------------------------------------------------------------
   function ucl_object_find_key (obj : access libucl.ucl_object_t;
                                 key : String) return access constant libucl.ucl_object_t
   is
      ckey   : ICS.chars_ptr;
      result : access constant libucl.ucl_object_t;
   begin
      ckey := ICS.New_String (key);
      result := libucl.ucl_object_lookup (obj, ckey);
      ICS.Free (ckey);
      return result;
   end ucl_object_find_key;

   --------------------------------------------------------------------
   --  ucl_object_typed_new_object
   --------------------------------------------------------------------
   function ucl_object_typed_new_object return access libucl.ucl_object_t is
   begin
      return libucl.ucl_object_typed_new (libucl.UCL_OBJECT);
   end ucl_object_typed_new_object;


   --------------------------------------------------------------------
   --  common_ucl_object_fromstring
   --------------------------------------------------------------------
   function common_ucl_object_fromstring (txt   : String;
                                          flags : libucl.ucl_string_flags)
                                          return access libucl.ucl_object_t
   is
      ctxt   : ICS.chars_ptr;
      result : access libucl.ucl_object_t;
   begin
      ctxt := ICS.New_String (txt);
      result := libucl.ucl_object_fromstring_common (ctxt, 0, flags);
      ICS.Free (ctxt);
      return result;
   end common_ucl_object_fromstring;


   --------------------------------------------------------------------
   --  ucl_object_fromstring_and_trim
   --------------------------------------------------------------------
   function ucl_object_fromstring_and_trim (txt : String) return access libucl.ucl_object_t is
   begin
      return (common_ucl_object_fromstring (txt, libucl.UCL_STRING_TRIM));
   end ucl_object_fromstring_and_trim;


   --------------------------------------------------------------------
   --  ucl_object_fromstring_and_trim
   --------------------------------------------------------------------
   function ucl_object_insert_key (top : access libucl.ucl_object_t;
                                   elt : access libucl.ucl_object_t;
                                   key : String;
                                   copy_key : Boolean) return Boolean
   is
      use type ICX.bool;

      ckey   : ICS.chars_ptr;
      ccopy  : ICX.bool := 0;
      result : ICX.bool;

   begin
      ckey := ICS.New_String (key);
      if copy_key then
         ccopy := 1;
      end if;
      result := libucl.ucl_object_insert_key (top      => top,
                                              elt      => elt,
                                              key      => ckey,
                                              keylen   => 0,
                                              copy_key => ccopy);
      ICS.Free (ckey);
      return (result = 1);
   end ucl_object_insert_key;

end Core.Ucl;
