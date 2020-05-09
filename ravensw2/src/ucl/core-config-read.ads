--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Core.Unix;
private with libucl;

package Core.Config.Read is

   Unsupported_Type : exception;

   --  Expand config_object into human-readable text, configuration format
   function config_dump return String;

   --  Overloaded functions that return value of configuration given enum.
   --  If the type is a mismatch then config_type_mismatch exception is thrown.
   function ucl_configuration_value (ci : Configuration_Item) return String;
   function ucl_configuration_value (ci : Configuration_Item) return Boolean;
   function ucl_configuration_value (ci : Configuration_Item) return int64;

   --  Initialize configuration
   --  Overlays ravensw.conf values over default values
   function establish_configuration
     (path     : String;
      reposdir : String;
      flags    : Init_protocol;
      dlevel   : ST_Debug_Level;
      options  : String) return Action_Result;

   --  Initialize configuration (not restricted to IPv4 or IPv6)
   function establish_configuration
     (path     : String;
      reposdir : String;
      dlevel   : ST_Debug_Level;
      options  : String) return Action_Result;

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

   function convert (obj : access constant libucl.ucl_object_t) return Config_Entry_Type;
   function convert_string_to_ucl_object (cetype  : Config_Entry_Type;
                                          payload : String) return access libucl.ucl_object_t;

   --  When provided a FIFO or socket, open file descriptor to it.
   --  Closed by Core.Finalize.cleanup
   procedure connect_evpipe (event_pipe : String);

   --  Pass open file descriptor to configuration file to parse it.
   --  Returns True if any fatal errors encountered
   function parse_configuration_file (conffd  : Unix.File_Descriptor;
                                      elf_abi : String) return Action_Result;

   --  Check all known configuration keys in the environment.
   --  If found, overwrite current value with environment's value
   procedure override_configuration_with_environment;

   --  Check pipe-delimited namepair settings from command line to override configuration
   procedure override_configuration_from_command_line (options : String);

   --  Check loaded repositories URL and protocol for validity.
   function check_repository_scheme_validity return Action_Result;

   --  Set debug level in context.  If dlevel > 0 then overwrite current configuration
   --  value (default < conf file < environment < CLI)
   procedure set_debug_level (dlevel : ST_Debug_Level);

end Core.Config.Read;
