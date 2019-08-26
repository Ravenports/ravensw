--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Cmd.Unset is

   --  This routine covers the case of ravensw executed without a command verb
   --  For example, to get version information or perform the activation status check
   function execute_no_command (comline : Cldata) return Boolean;

   --  Return False if initialization fails
   function Initialize_ravensw (comline : Cldata) return Boolean;

private

   --  ravensw -v
   function basic_version_info return Boolean;

   --  ravensw -vv
   function extended_version_info return Boolean;

   --  Provide uniform configuration formatting
   function format_extconfig (name, value : String; quotes, last : Boolean) return String;

   --  Print result of format_extconfig
   procedure print_extconfig (name, value : String; quotes : Boolean; last : Boolean := False);

   --  ravensw -l
   function list_available_commands return Boolean;

   --  ravensw -N
   function do_activation_test return Boolean;

   --  part of ravensw -vv
   procedure show_repository_info;

end Cmd.Unset;
