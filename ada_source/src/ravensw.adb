--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: License.txt

with Cmd.Line;

procedure Ravensw
is

   package CL renames Cmd.Line;

   comline_inputs : Cmd.Cldata;

begin
   comline_inputs := CL.parse_command_line;

end Ravensw;
