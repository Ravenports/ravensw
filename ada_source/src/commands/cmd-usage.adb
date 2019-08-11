--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;

package body Cmd.Usage is

   package TIO renames Text_IO;

   --------------------------------------------------------------------
   --  command_line_valid
   --------------------------------------------------------------------
   function command_line_valid (comline : Cldata) return Boolean
   is
   begin
      case comline.command is
         when cv_unset => return no_command_verb (comline);
      end case;
   end command_line_valid;


   --------------------------------------------------------------------
   --  display_error
   --------------------------------------------------------------------
   procedure display_error (error_msg: Text) is
   begin
      TIO.Put_Line (progname & ": " & error_msg);
   end display_error;


   --------------------------------------------------------------------
   --  no_command_verb
   --------------------------------------------------------------------
   function no_command_verb (comline : Cldata) return Boolean is
   begin
      if comline.parse_error then
         display_error;
         return False;
      else
         --  check if no arguments given
         if not comline.glob_debug and then
           not comline.glob_list and then
           not comline.glob_status_check and then
           comline.glob_version = T_version'First and then
           IsBlank (comline.glob_chroot) and then
           IsBlank (comline.glob_config_file) and then
           IsBlank (comline.glob_repo_config_dir) and then
           IsBlank (comline.glob_root_dir) and then
           IsBlank (comline.glob_option) and then
           IsBlank (comline.glob_jail) and then
           comline.global_init_flags = init_none
         then
            display_error ("Not enough arguments");
            return False;
         end if;

         --  check if jail requested when unsupported
         if not jail_supported and then not IsBlank (comline.glob_jail) then
            display_error ("Jail operation is not supported on this platform");
            return False;
         end if;

         --  Only two switches used without a command verb
         if comline.glob_version = T_version'First and then
           not comline.glob_status_check
         then
            display_error ("No commands specified");
            return False;
         end if;

         return True;
      end if;
   end no_command_verb;

end Cmd.Usage;
