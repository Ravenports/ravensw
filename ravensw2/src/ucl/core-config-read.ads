--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with libucl;


package Core.Config.Read is

   --  Expand config_object into human-readable text, configuration format
   function config_dump return String;

   --  Overloaded functions that return value of configuration given enum.
   --  If the type is a mismatch then config_type_mismatch exception is thrown.
   function ucl_configuration_value (ci : Configuration_Item) return String;
   function ucl_configuration_value (ci : Configuration_Item) return Boolean;
   function ucl_configuration_value (ci : Configuration_Item) return int64;

private

   config_object : access libucl.ucl_object_t;
   parsed        : Boolean := False;

   --  Retrieve configuration object given its key
   function config_get (key : String) return access constant libucl.ucl_object_t;

   --  Retrieve string converted from ucl object given its key`
   function config_get_string (key : String) return String;

   --  Retrieve boolean converted from ucl object given its key`
   function config_get_boolean (key : String) return Boolean;

   --  Retrieve signed 64-bit integer converted from ucl object given its key`
   function config_get_int64 (key : String) return int64;

end Core.Config.Read;
