--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core; use Core;

with Ada.Text_IO;
private with Ada.Containers.Vectors;

package Cmd is

   package TIO renames Ada.Text_IO;

   type Cldata is private;

private

   package CON renames Ada.Containers;

   type T_Search_Modifiers is
      record
         annotations     : Boolean;
         arch            : Boolean;
         categories      : Boolean;
         comment         : Boolean;
         depends_on      : Boolean;
         description     : Boolean;
         full            : Boolean;
         licenses        : Boolean;
         maintainer      : Boolean;
         name            : Boolean;
         options         : Boolean;
         pkg_size        : Boolean;
         prefix          : Boolean;
         repository      : Boolean;
         required_by     : Boolean;
         shlibs_required : Boolean;
         shlibs_provided : Boolean;
         size            : Boolean;
         url             : Boolean;
         version         : Boolean;
         www             : Boolean;
      end record;

   type Pkg_init_flags is
     (init_none,
      init_use_ipv4,
      init_use_ipv6);

   --  Commands in pkg(8) not supported (intentionally)
   --  cv_audit
   --  cv_bootstrap
   --  cv_convert
   --  cv_updating
   --  cv_register
   --  cv_plugins

   type Command_verb is
     (cv_unset,
      cv_add,
      cv_alias,
      cv_annotate,
      cv_autoremove,
      cv_backup,
      cv_check,
      cv_clean,
      cv_config,
      cv_create,
      cv_delete,
      cv_fetch,
      cv_help,
      cv_info,
      cv_install,
      cv_lock,
      cv_query,
      cv_remove,
      cv_repo,
      cv_rquery,
      cv_search,
      cv_set,
      cv_shell,
      cv_shlib,
      cv_ssh,
      cv_stats,
      cv_unlock,
      cv_update,
      cv_upgrade,
      cv_version,
      cv_which
     );

   type T_Check_Action is
     (unset_action,
      shared_libraries,
      locate_missing_deps,
      recompute_checksums,
      invalid_checksums
     );

   type T_Flag is
     (undefined_flag,
      flag_off,
      flag_on);

   type T_Shlib is
     (undefined_shlib,
      provider,
      consumer
     );

   type T_statistics is
     (no_database_stats,
      local_database,
      remote_database,
      both_databases
     );

   type T_version is
     (no_defined_behavior,
      use_remote_catalog_state,
      use_conspiracy_state,
      test_versions,
      compare_against_pattern
     );

   type T_Annotate_Action is
     (no_annotate_action,
      add,
      modify,
      show,
      delete);

   type T_Search_Field is
     (no_search_field,
      comment,
      description,
      name,
      origin,
      package_name);

   type T_Raw_Format is
     (no_raw_format,
      json,
      json_compact,
      ucl,
      yaml);

   subtype ST_Debug_Level is Natural range 0 .. 4;
   subtype ST_Version is Natural range 0 .. 2;
   subtype ST_Modifier_Index is Natural range 0 .. 20;

   package string_crate is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");

   type Cldata is
      record
         glob_debug           : ST_Debug_Level := 0;
         glob_version         : ST_Version := 0;
         glob_chroot          : Text;
         glob_config_file     : Text;
         glob_repo_config_dir : Text;
         glob_root_dir        : Text;
         glob_option          : Text;
         glob_jail            : Text;
         glob_list            : Boolean := False;
         glob_status_check    : Boolean := False;
         global_init_flags    : Pkg_init_flags := init_none;
         command              : Command_verb := cv_unset;
         parse_error          : Boolean := False;

         help_command         : Command_verb := cv_unset;

         verb_quiet           : Boolean := False;
         verb_assume_yes      : Boolean := False;
         verb_shell_glob      : Boolean := False;
         verb_use_regex       : Boolean := False;
         verb_case_sensitive  : Boolean := False;
         verb_case_blind      : Boolean := False;
         verb_all_packages    : Boolean := False;
         verb_dry_run         : Boolean := False;
         verb_verbose         : Boolean := False;
         verb_skip_catalog    : Boolean := False;
         verb_name_pattern    : Text;
         verb_repo_name       : Text;
         verb_raw_format      : T_Raw_Format := no_raw_format;
         verb_work_queue      : string_crate.Vector;

         --  No unique configuration
         --  * autoremove
         --  * clean

         add_skip_scripts     : Boolean := False;
         add_mark_automatic   : Boolean := False;
         add_force_reinstall  : Boolean := False;
         add_accept_missing   : Boolean := False;

         alias_list           : Boolean := False;

         annot_action         : T_Annotate_Action := no_annotate_action;
         annot_tag_name       : Text;
         annot_tag_value      : Text;

         backup_dump          : Text;
         backup_restore       : Text;

         check_dry_run        : Boolean := False;
         check_action         : T_Check_Action := unset_action;

         config_key           : Text;

         create_root_dir      : Text;
         create_output_dir    : Text;
         create_metadata_dir  : Text;
         create_manifest_file : Text;
         create_plist_file    : Text;
         create_ban_overwrite : Boolean := False;

         delete_skip_script   : Boolean := False;
         delete_force         : Boolean := False;
         delete_rev_deps_too  : Boolean := False;

         fetch_deps_too       : Boolean := False;
         fetch_avail_updates  : Boolean := False;
         fetch_destdir        : Text;

         info_show_annotation : Boolean := False;
         info_full            : Boolean := False;
         info_raw_manifest    : Boolean := False;
         info_alter_return    : Boolean := False;
         info_pkg_message     : Boolean := False;
         info_fwd_deps        : Boolean := False;
         info_rev_deps        : Boolean := False;
         info_lock_status     : Boolean := False;
         info_installed_by    : Boolean := False;
         info_shlibs_provided : Boolean := False;
         info_shlibs_used     : Boolean := False;
         info_total_size      : Boolean := False;
         info_search_origin   : Boolean := False;
         info_origin          : Boolean := False;
         info_install_prefix  : Boolean := False;
         info_comment         : Boolean := False;
         info_list_files      : Boolean := False;
         info_file_archive    : Text;

         inst_mark_automatic  : Boolean := False;
         inst_force_reinstall : Boolean := False;
         inst_skip_scripts    : Boolean := False;
         inst_ignore_missing  : Boolean := False;
         inst_fetch_only      : Boolean := False;
         inst_force_rev_deps  : Boolean := False;

         query_eval_condition : Text;
         query_file_archive   : Text;
         query_format         : Text;

         repo_list            : Boolean := False;
         repo_output_dir      : Text;
         repo_meta_file       : Text;
         repo_path            : Text;
         repo_rsa_key         : Text;
         repo_signing_cmd     : Text;

         rquery_index_line    : Boolean := False;
         rquery_query_format  : Text;
         rquery_eval_cond     : Text;

         search_exact_match   : Boolean := False;
         search_raw_manifest  : Boolean := False;
         search_label         : T_Search_Field;
         search_field         : T_Search_Field;
         search_modifiers     : T_Search_Modifiers;

         set_automatic_flag   : T_Flag := undefined_flag;
         set_vital_flag       : T_Flag := undefined_flag;
         set_name_pair        : Text;

         shlib_behavior       : T_Shlib := undefined_shlib;

         stats_behavior       : T_statistics := no_database_stats;
         stats_bytes_only     : Boolean := False;

         update_force         : Boolean := False;

         upgrade_force        : Boolean := False;
         upgrade_skip_scripts : Boolean := False;
         upgrade_fetch_only   : Boolean := False;

         version_behavior     : T_version := no_defined_behavior;
         version_exact_match  : Boolean := False;
         version_disp_origin  : Boolean := False;
         version_match_char   : Character := Character'First;
         version_not_char     : Character := Character'First;
         version_test1        : Text;
         version_test2        : Text;
         version_origin       : Text;
         version_pkg_name     : Text;

         which_origin         : Boolean := False;
         which_show_match     : Boolean := False;
         which_path_search    : Boolean := False;
         which_filename       : Text;

         lock_show_locked     : Boolean := False;
         lock_set_exitcode    : Boolean := False;

         shell_pass_along     : Text;

         error_message        : Text;

      end record;

   --  Provide string equivalent to given command enumeration
   function convert_command_enum_to_label (command : Command_verb) return String;

   --  Provide string equivalent to given search and label field enumeration
   function convert_search_enum_to_label (field : T_Search_Field) return String;

   --  Provide string equivalent to given search modifier enumeration
   function convert_modifier_enum_to_label (index : ST_Modifier_Index) return String;

end Cmd;
