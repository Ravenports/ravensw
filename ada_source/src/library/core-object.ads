--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with libucl;
with Core.Ucl;

package Core.Object is

   function pkg_object_string (obj : access constant libucl.ucl_object_t) return String;
   function pkg_object_int (obj : access constant libucl.ucl_object_t) return Ucl.int64;
   function pkg_object_bool (obj : access constant libucl.ucl_object_t) return Boolean;
   function pkg_object_dump (obj : access constant libucl.ucl_object_t) return String;

end Core.Object;
