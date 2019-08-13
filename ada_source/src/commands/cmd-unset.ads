--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Cmd.Unset is

   --  This routine covers the case of ravensw executed without a command verb
   --  For example, to get version information or perform the activation status check
   function execute_no_command (comline : Cldata) return Boolean;

private

   --  ravensw -v
   function basic_version_info return Boolean;

   --  ravensw -vv
   function extended_version_info return Boolean;

   --  Provide uniform configuration formatting
   function format_extconfig (name, value : String) return String;

   --  ravensw -l
   function list_commands return Boolean;


end Cmd.Unset;
