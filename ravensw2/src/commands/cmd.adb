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
         when cv_remove     => return "remove";
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

   --------------------------------------------------------------------
   --  convert_search_enum_to_label
   --------------------------------------------------------------------
   function convert_search_enum_to_label (field : A_Search_Field) return String
   is
   begin
      case field is
         when no_search_field => return "";
         when comment         => return "comment";
         when description     => return "description";
         when name            => return "name";
         when origin          => return "origin";
         when package_name    => return "pkg-name";
      end case;
   end convert_search_enum_to_label;


   --------------------------------------------------------------------
   --  convert_modifier_enum_to_label
   --------------------------------------------------------------------
   function convert_modifier_enum_to_label (index : ST_Modifier_Index) return String
   is
   begin
      case index is
         when  0 => return "annotations";
         when  1 => return "arch";
         when  2 => return "categories";
         when  3 => return "comment";
         when  4 => return "depends-on";
         when  5 => return "description";
         when  6 => return "full";
         when  7 => return "licenses";
         when  8 => return "maintainer";
         when  9 => return "name";
         when 10 => return "options";
         when 11 => return "pkg-size";
         when 12 => return "prefix";
         when 13 => return "repository";
         when 14 => return "required-by";
         when 15 => return "shared-libs-required";
         when 16 => return "shared-libs-provided";
         when 17 => return "size";
         when 18 => return "url";
         when 19 => return "version";
         when 20 => return "www";
      end case;
   end convert_modifier_enum_to_label;


end Cmd;
