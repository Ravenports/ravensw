--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Cmd is

   --------------------------------------------------------------------
   --  convert_command_enum_to_label
   --------------------------------------------------------------------
   function convert_command_enum_to_label (command : Command_verb) return String
   is
   begin
      case command is
         when cv_unset      => return "";
         when cv_add        => return "add";
         when cv_alias      => return "alias";
         when cv_annotate   => return "annotate";
         when cv_autoremove => return "autoremove";
         when cv_backup     => return "backup";
         when cv_check      => return "check";
         when cv_clean      => return "clean";
         when cv_config     => return "config";
         when cv_create     => return "create";
         when cv_delete     => return "delete";
         when cv_fetch      => return "fetch";
         when cv_help       => return "help";
         when cv_info       => return "info";
         when cv_install    => return "install";
         when cv_lock       => return "lock";
         when cv_query      => return "query";
         when cv_repo       => return "repo";
         when cv_rquery     => return "rquery";
         when cv_search     => return "search";
         when cv_set        => return "set";
         when cv_shell      => return "shell";
         when cv_shlib      => return "shlib";
         when cv_ssh        => return "ssh";
         when cv_stats      => return "stats";
         when cv_unlock     => return "unlock";
         when cv_update     => return "update";
         when cv_upgrade    => return "upgrade";
         when cv_version    => return "version";
         when cv_which      => return "which";
      end case;
   end convert_command_enum_to_label;

end Cmd;
