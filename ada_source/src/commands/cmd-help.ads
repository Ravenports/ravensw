--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Cmd.Help is

   --  Executes help command
   function execute_help_command (comline : Cldata) return Boolean;

private

   procedure print_global_options;
   procedure print_command_summary;

   --  Provide uniform formatting
   procedure PL (name, value : String);

   --  Launch /raven/bin/man or /usr/bin/man to display man page of command
   --  Return true on success
   function show_man_page (command : Command_verb) return Boolean;

end Cmd.Help;
