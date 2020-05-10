--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Cmd.Unset;
with Cmd.Help;
with Cmd.Version;
with Cmd.Shell;

package body Cmd.Bahnhof is

   package C00 renames Cmd.Unset;
   package C01 renames Cmd.Help;
   package C02 renames Cmd.Version;
   package C03 renames Cmd.Shell;

   --------------------------------------------------------------------
   --  execute_command
   --------------------------------------------------------------------
   function execute_command (comline : Cldata) return Boolean is
   begin
      case comline.command is
         when cv_unset => null;
         when cv_help  => null;
         when others =>
            if not C00.Initialize_ravensw (comline) then
               return False;
            end if;
      end case;

      case comline.command is
         when cv_unset   => return (C00.execute_no_command (comline));
         when cv_help    => return (C01.execute_help_command (comline));
         when cv_version => return (C02.execute_version_command (comline));
         when cv_shell   => return (C03.execute_shell_command (comline));
         when others =>
            TIO.Put_Line ("Command '" & convert_command_enum_to_label (comline.command) &
                            "' hasn't been implemented yet.  Sorry!");
            return False;
      end case;
   end execute_command;

end Cmd.Bahnhof;
