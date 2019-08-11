--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Cmd.Usage is

   --  Check if error occurred during command line parsing
   --  If not, return true.   If so, show error and dynamic usage.
   function command_line_valid (comline : Cldata) return Boolean;

private

   --  Common routine to display error message before usage.
   procedure display_error (error_msg: Text);

   --  Break each command into individual routines.  Any additional
   --  validation checks (if no parsing error exists) can be done here.
   function no_command_verb (comline : Cldata) return Boolean;

end Cmd.Usage;
