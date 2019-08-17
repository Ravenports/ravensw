--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with libucl;

package Core.Ucl is


   function ucl_object_find_key (obj : access libucl.ucl_object_t;
                                 key : String) return access constant libucl.ucl_object_t;

   --  Equivalent: ucl_object_typed_new(UCL_OBJECT)
   function ucl_object_typed_new_object return access libucl.ucl_object_t;

   --  Equivalent: ucl_object_typed_new(UCL_ARRAY)
   function ucl_object_typed_new_array return access libucl.ucl_object_t;

   --  Equivalent: ucl_object_fromstring_common (string, size, UCL_STRING_TRIM)
   --  creates and object from text (and trim transform)
   function ucl_object_fromstring_and_trim (txt : String) return access libucl.ucl_object_t;

   --  Equivalent: ucl_object_fromstring_common (string, size, UCL_STRING_PARSE_BOOLEAN)
   --  creates and object from text (and trim transform)
   function ucl_object_fromstring_boolean (txt : String) return access libucl.ucl_object_t;

   --  Equivalent: ucl_object_fromstring_common (string, size, UCL_STRING_PARSE_INT)
   --  creates and object from text (and trim transform)
   function ucl_object_fromstring_int (txt : String) return access libucl.ucl_object_t;

   function ucl_object_insert_key (top : access libucl.ucl_object_t;
                                   elt : access libucl.ucl_object_t;
                                   key : String;
                                   copy_key : Boolean) return Boolean;

   --  Returns true if value was inserted into array
   function ucl_array_push (top : access libucl.ucl_object_t;
                            elt : access libucl.ucl_object_t) return Boolean;


private

   --  internal use
   function common_ucl_object_fromstring (txt   : String;
                                          flags : libucl.ucl_string_flags)
                                          return access libucl.ucl_object_t;

end Core.Ucl;
