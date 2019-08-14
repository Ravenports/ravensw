--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with libucl;

package Core.Ucl is

   type pkg_object is access constant libucl.ucl_object_t;

   function ucl_object_find_key (obj : pkg_object;
                                 key : String) return pkg_object;

end Core.Ucl;
