--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: License.txt

with Ada.Command_Line;
with Cmd.Line;
with Cmd.Usage;

procedure Ravensw
is

   package ACL renames Ada.Command_Line;
   package CL  renames Cmd.Line;
   package CU  renames Cmd.Usage;

   comline_inputs : Cmd.Cldata;

begin
   comline_inputs := CL.parse_command_line;
   if not CU.command_line_valid (comline_inputs) then
      ACL.Set_Exit_Status (Code => ACL.Failure);
   end if;

   ACL.Set_Exit_Status (Code => ACL.Success);

end Ravensw;
