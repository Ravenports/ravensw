--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with GNAT.Exception_Traces;
with Ada.Command_Line;
with Cmd.Line;
with Cmd.Usage;
with Cmd.Bahnhof;
with Core.Finalize;
with Libfetch;

procedure Main
is

   package ACL renames Ada.Command_Line;
   package CB  renames Cmd.Bahnhof;
   package CL  renames Cmd.Line;
   package CU  renames Cmd.Usage;
   package FIN renames Core.Finalize;
   package GET renames GNAT.Exception_Traces;

   comline_inputs : Cmd.Cldata;

begin
   --  This can be removed after debugging (?)
   GET.Trace_On (GET.Every_Raise);

   Libfetch.initialize_estreams;
   comline_inputs := CL.parse_command_line;
   if not CU.command_line_valid (comline_inputs) then
      ACL.Set_Exit_Status (Code => ACL.Failure);
      return;
   end if;

   --  TODO: establish jail in context

   if CB.execute_command (comline_inputs) then
      ACL.Set_Exit_Status (Code => ACL.Success);
   else
      ACL.Set_Exit_Status (Code => ACL.Failure);
   end if;

   FIN.cleanup;

end Main;
