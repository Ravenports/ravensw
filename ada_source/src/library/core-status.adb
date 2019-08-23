--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with GNAT.OS_Lib;
with Core.Config;

package body Core.Status is

   package DIR renames Ada.Directories;
   package OSL renames GNAT.OS_Lib;

   --------------------------------------------------------------------
   --  pkg_status
   --------------------------------------------------------------------
   function pkg_status return Pkg_Status_Output
   is
      result : Pkg_Status_Output;
      dbdir  : constant String := Config.pkg_config_get_string (Config.conf_dbdir);
      dbfile : constant String := dbdir & "/local.sqlite";
   begin
      result.count := 0;

      if not OSL.Is_Read_Accessible_File (dbfile) then
         result.status := PKG_STATUS_NODB;
         return result;
      end if;


      return result;
   end pkg_status;

end Core.Status;
