--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Config is

   --------------------------------------------------------------------
   --  pkg_initialized
   --------------------------------------------------------------------
   function pkg_initialized return Boolean is
   begin
      return parsed;
   end pkg_initialized;


   --------------------------------------------------------------------
   --  pkg_init
   --------------------------------------------------------------------
   function pkg_init
     (path     : String;
      reposdir : String) return Pkg_Error_Type is
   begin
      return pkg_ini (path, reposdir, init_none);
   end pkg_init;

end Core.Config;
