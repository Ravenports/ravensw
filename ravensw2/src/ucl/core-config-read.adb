--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Object;
with Ucl;


package body Core.Config.Read is

   package COB renames Core.Object;

   --------------------------------------------------------------------
   --  config_get
   --------------------------------------------------------------------
   function config_get (key : String) return access constant libucl.ucl_object_t is
   begin
      return Ucl.ucl_object_find_key (config_object, key);
   end config_get;


   --------------------------------------------------------------------
   --  config_get_string
   --------------------------------------------------------------------
   function config_get_string (key : String) return String is
   begin
      return COB.pkg_object_string (config_get (key));
   end config_get_string;


   --------------------------------------------------------------------
   --  config_get_boolean
   --------------------------------------------------------------------
   function config_get_boolean (key : String) return Boolean is
   begin
      return COB.pkg_object_bool (config_get (key));
   end config_get_boolean;


   --------------------------------------------------------------------
   --  config_get_int64
   --------------------------------------------------------------------
   function config_get_int64 (key : String) return int64 is
   begin
      return COB.pkg_object_int (config_get (key));
   end config_get_int64;


   --------------------------------------------------------------------
   --  config_dump
   --------------------------------------------------------------------
   function config_dump return String is
   begin
      return Object.pkg_object_dump (config_object);
   end config_dump;


   --------------------------------------------------------------------
   --  ucl_configuration_value #1 (Boolean result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return Boolean is
   begin
      if config_get_type (ci) = pkg_bool then
         return config_get_boolean (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


   --------------------------------------------------------------------
   --  ucl_configuration_value #2 (int64 result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return int64 is
   begin
      if config_get_type (ci) = pkg_int then
         return config_get_int64 (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


   --------------------------------------------------------------------
   --  ucl_configuration_value #3 (string result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return String is
   begin
      if config_get_type (ci) = pkg_string then
         return config_get_string (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


end Core.Config.Read;
