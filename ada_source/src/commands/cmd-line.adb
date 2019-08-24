--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with Ada.Characters.Latin_1;

with Core.Strings; use Core.Strings;

package body Cmd.Line is

   package ACL renames Ada.Command_Line;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  parse_command_line
   --------------------------------------------------------------------
   function parse_command_line return Cldata
   is
      --  NV arguments
      type Clswitch is
        (nothing_pending,
         global_chroot,
         global_config,
         global_repoconfdir,
         global_rootdir,
         global_option,
         global_jail,
         generic_repo_name,
         generic_raw_format,
         annotate_tag,
         backup_destination,
         backup_source,
         create_metadatadir,
         create_manifest,
         create_outdir,
         create_rootdir,
         create_plist,
         fetch_destdir,
         help,
         info_archive_file,
         lock_pkgname,
         query_condition,
         query_filename,
         repo_meta_File,
         repo_outdir,
         repo_signing_cmd,
         rquery_eval_cond,
         search_modifier,
         search_field,
         search_label,
         set_automatic,
         set_vital,
         set_change_name,
         version_match_char,
         version_not_char,
         version_origin,
         version_pkgname,
         which_filename
        );

      procedure expand_command_line;
      procedure translate_switch (position : string_crate.Cursor);
      function get_command (component : String) return Command_verb;
      function aCgix (datum : String; datumtxt : Text; use_all : Boolean := True) return Boolean;
      procedure handle_search_modifier (datum : String);
      procedure handle_search_field (datum : String);
      procedure handle_label_field (datum : String);
      procedure handle_raw_format (datum : String);
      procedure handle_set_automatic (datum : String);
      procedure handle_set_vital (datum : String);
      procedure handle_trailing_pkgname (datum : String; datumtxt : Text);
      procedure set_error (error_msg : String);
      procedure check_annotate_stdin;
      procedure check_create_incompatibilities;
      procedure check_implied_info_all;
      procedure check_implied_query_all;
      procedure check_implied_rquery_all;
      procedure check_stats_default;

      expanded_args : string_crate.Vector;
      result        : Cldata;
      last_cmd      : Clswitch := nothing_pending;

      search_comment     : constant String := "comment";
      search_description : constant String := "description";
      search_name        : constant String := "name";
      search_origin      : constant String := "origin";
      search_pkgname     : constant String := "pkg-name";

      ---------------------------------
      --  expand_command_line
      ---------------------------------
      procedure set_error (error_msg : String) is
      begin
         --  Don't overwrite previous errors
         if not result.parse_error then
            result.parse_error := True;
            result.error_message := SUS (error_msg);
         end if;
      end set_error;

      ---------------------------------
      --  handle_search_modifier
      ---------------------------------
      procedure handle_search_modifier (datum : String) is
      begin
         if datum = "annotations" then
            result.search_modifiers.annotations := True;
         elsif datum = "arch" then
            result.search_modifiers.arch := True;
         elsif datum = "categories" then
            result.search_modifiers.categories := True;
         elsif datum = "comment" then
            result.search_modifiers.comment := True;
         elsif datum = "depends-on" then
            result.search_modifiers.depends_on := True;
         elsif datum = "description" then
            result.search_modifiers.description := True;
         elsif datum = "full" then
            result.search_modifiers.full := True;
         elsif datum = "licenses" then
            result.search_modifiers.licenses := True;
         elsif datum = "maintainer" then
            result.search_modifiers.maintainer := True;
         elsif datum = "name" then
            result.search_modifiers.name := True;
         elsif datum = "options" then
            result.search_modifiers.options := True;
         elsif datum = "pkg-size" then
            result.search_modifiers.pkg_size := True;
         elsif datum = "prefix" then
            result.search_modifiers.prefix := True;
         elsif datum = "repository" then
            result.search_modifiers.repository := True;
         elsif datum = "required-by" then
            result.search_modifiers.required_by := True;
         elsif datum = "shared-libs-required" then
            result.search_modifiers.shlibs_required := True;
         elsif datum = "shared-libs-provided" then
            result.search_modifiers.shlibs_provided := True;
         elsif datum = "size" then
            result.search_modifiers.size := True;
         elsif datum = "url" then
            result.search_modifiers.url := True;
         elsif datum = "version" then
            result.search_modifiers.version := True;
         elsif datum = "www" then
            result.search_modifiers.www := True;
         else
            set_error ("Unknown modifier option: " & datum);
         end if;
      end handle_search_modifier;

      ---------------------------------
      --  handle_search_field
      ---------------------------------
      procedure handle_search_field (datum : String) is
      begin
         if result.search_field = no_search_field then
            if datum = search_comment then
               result.search_field := comment;
            elsif datum = search_description then
               result.search_field := description;
            elsif datum = search_name then
               result.search_field := name;
            elsif datum = search_origin then
               result.search_field := origin;
            elsif datum = search_pkgname then
               result.search_field := package_name;
            else
               set_error ("Unknown search option: " & datum);
            end if;
         else
            set_error ("Attempt to redefine search field to '" & datum & "'");
         end if;
      end handle_search_field;

      ---------------------------------
      --  handle_label_field
      ---------------------------------
      procedure handle_label_field (datum : String) is
      begin
         if result.search_label = no_search_field then
            if datum = search_comment then
               result.search_label := comment;
            elsif datum = search_description then
               result.search_label := description;
            elsif datum = search_name then
               result.search_label := name;
            elsif datum = search_origin then
               result.search_label := origin;
            elsif datum = search_pkgname then
               result.search_label := package_name;
            else
               set_error ("Unknown label option: " & datum);
            end if;
         else
            set_error ("Attempt to redefine label to '" & datum & "'");
         end if;
      end handle_label_field;


      ---------------------------------
      --  handle_raw_format
      ---------------------------------
      procedure handle_raw_format (datum : String) is
      begin
         if datum = "json" then
            result.verb_raw_format := json;
         elsif datum = "json-compact" then
            result.verb_raw_format := json_compact;
         elsif datum = "yaml" then
            result.verb_raw_format := yaml;
         elsif datum = "ucl" then
            result.verb_raw_format := ucl;
         else
            set_error ("Invalid format '" & datum & "' for the raw output " &
                         "(expecting json, json-compact, yaml or ucl)");
         end if;
      end handle_raw_format;


      ---------------------------------
      --  handle_set_automatic
      ---------------------------------
      procedure handle_set_automatic (datum : String) is
      begin
         if result.set_automatic_flag = undefined_flag then
            if datum = "0" then
               result.set_automatic_flag := flag_off;
            elsif datum = "1" then
               result.set_automatic_flag := flag_on;
            else
               set_error ("Wrong value for --automatic switch. Expecting 0 or 1, got: " & datum);
            end if;
         else
            set_error ("Attempt to redefine --automatic switch: " & datum);
         end if;
      end handle_set_automatic;


      ---------------------------------
      --  handle_set_vital
      ---------------------------------
      procedure handle_set_vital (datum : String) is
      begin
         if result.set_vital_flag = undefined_flag then
            if datum = "0" then
               result.set_vital_flag := flag_off;
            elsif datum = "1" then
               result.set_vital_flag := flag_on;
            else
               set_error ("Wrong value for --vital switch. Expecting 0 or 1, got: " & datum);
            end if;
         else
            set_error ("Attempt to redefine --vital switch: " & datum);
         end if;
      end handle_set_vital;


      ---------------------------------
      --  aCgix
      ---------------------------------
      function aCgix (datum : String; datumtxt : Text; use_all : Boolean := True) return Boolean
      is
         sws_all    : constant String := "-a";
         swl_all    : constant String := "--all";
         sws_case   : constant String := "-C";
         swl_case   : constant String := "--case-sensitive";
         sws_glob   : constant String := "-g";
         swl_glob   : constant String := "--glob";
         sws_icase  : constant String := "-i";
         swl_icase  : constant String := "--case-insensitive";
         sws_regex  : constant String := "-x";
         swl_regex  : constant String := "--regex";
      begin
         if use_all and (datum = sws_all or else datum = swl_all) then
            result.verb_all_packages := True;
         elsif datum = sws_case or else datum = swl_case then
            result.verb_case_sensitive := True;
         elsif datum = sws_icase or else datum = swl_icase then
            result.verb_case_blind := True;
         elsif datum = sws_glob or else datum = swl_glob then
            result.verb_shell_glob := True;
         elsif datum = sws_regex or else datum = swl_regex then
            result.verb_use_regex := True;
         else
            return False;
         end if;
         return True;
      end aCgix;


      ---------------------------------
      --  handle_trailing_pkgname
      ---------------------------------
      procedure handle_trailing_pkgname (datum : String; datumtxt : Text)
      is
         hyphen : constant Character := '-';
      begin
         if datum (datum'First) = hyphen then
            set_error ("Unexpected switch: " & datum);
         else
            if result.verb_all_packages then
               set_error ("Unexpected file pattern with --all option: " & datum);
            elsif not IsBlank (result.verb_name_pattern) then
               set_error ("Attempt to redefine file name pattern from '" &
                 USS (result.verb_name_pattern) & "' to '" & datum & "'");
            else
               result.verb_name_pattern := datumtxt;
            end if;
         end if;
      end handle_trailing_pkgname;


      ---------------------------------
      --  expand_command_line
      ---------------------------------
      procedure expand_command_line is
      begin
         for Arg in 1 .. ACL.Argument_Count loop
            declare
               datum      : String := ACL.Argument (Arg);
               save_as_is : Boolean := False;
            begin
               if datum'Length = 1 then
                  --  Too small to be any kind of switch, just save it
                  save_as_is := True;
               elsif datum (datum'First) = '-' then
                  if datum (datum'First + 1) = '-' then
                     if datum'Length = 2 then
                        --  illegal "--" value, just save
                        save_as_is := True;
                     else
                        --  full switch, check for NV
                        if contains (datum, "=") then
                           --  Save both sides separately
                           expanded_args.Append (SUS (part_1 (datum, "=")));
                           expanded_args.Append (SUS (part_2 (datum, "=")));
                        else
                           --  not in NV format, just save it
                           save_as_is := True;
                        end if;
                     end if;
                  else
                     --  more or more short switches concatenated, expand
                     for ch in datum'First + 1 .. datum'Last loop
                        expanded_args.Append (SUS ('-' & datum (ch)));
                     end loop;
                  end if;
               else
                  --  No switch prefix, save without expanding
                  save_as_is := True;
               end if;
               if save_as_is then
                  expanded_args.Append (SUS (datum));
               end if;
            end;
         end loop;
      end expand_command_line;


      ---------------------------------
      --  translate_switch
      ---------------------------------
      function get_command (component : String) return Command_verb
      is
         total_keywords : constant Positive := Command_verb'Pos (Command_verb'Last) + 1;

         subtype keyword_string is String (1 .. 10);

         type keyword_pair is
            record
               keyword : keyword_string;
               keytype : Command_verb;
            end record;

         --  Keep in alphabetical order (critical!)
         all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
           (
            ("NOTFOUND  ", cv_unset),
            ("add       ", cv_add),
            ("alias     ", cv_alias),
            ("annotate  ", cv_annotate),
            ("autoremove", cv_autoremove),
            ("backup    ", cv_backup),
            ("check     ", cv_check),
            ("clean     ", cv_clean),
            ("config    ", cv_config),
            ("create    ", cv_create),
            ("delete    ", cv_delete),
            ("fetch     ", cv_fetch),
            ("help      ", cv_help),
            ("info      ", cv_info),
            ("install   ", cv_install),
            ("lock      ", cv_lock),
            ("query     ", cv_query),
            ("remove    ", cv_remove),
            ("repo      ", cv_repo),
            ("rquery    ", cv_rquery),
            ("search    ", cv_search),
            ("set       ", cv_set),
            ("shell     ", cv_shell),
            ("shlib     ", cv_shlib),
            ("ssh       ", cv_ssh),
            ("stats     ", cv_stats),
            ("unlock    ", cv_unlock),
            ("update    ", cv_update),
            ("upgrade   ", cv_upgrade),
            ("version   ", cv_version),
            ("which     ", cv_which)
           );

         bandolier : keyword_string := (others => ' ');
         Low       : Natural := all_keywords'First;
         High      : Natural := all_keywords'Last;
         Mid       : Natural;
      begin
         if component'Length > keyword_string'Length or else
           component'Length < 3
         then
            return cv_unset;
         end if;

         bandolier (1 .. component'Length) := component;

         loop
            Mid := (Low + High) / 2;
            if bandolier = all_keywords (Mid).keyword  then
               return all_keywords (Mid).keytype;
            elsif bandolier < all_keywords (Mid).keyword then
               exit when Low = Mid;
               High := Mid - 1;
            else
               exit when High = Mid;
               Low := Mid + 1;
            end if;
         end loop;
         return cv_unset;

      end get_command;


      ---------------------------------
      --  translate_switch
      ---------------------------------
      procedure translate_switch (position : string_crate.Cursor)
      is
         datumtxt : Text renames string_crate.Element (position);
         datum : constant String := USS (datumtxt);

         sws_quiet  : constant String := "-q";
         swl_quiet  : constant String := "--quiet";
         sws_glob   : constant String := "-g";
         swl_glob   : constant String := "--glob";
         sws_origin : constant String := "-o";
         swl_origin : constant String := "--origin";
         sws_dryrun : constant String := "-n";
         swl_dryrun : constant String := "--dry-run";
         sws_yes    : constant String := "-y";
         swl_yes    : constant String := "--yes";
         sws_all    : constant String := "-a";
         swl_all    : constant String := "--all";
         sws_regex  : constant String := "-x";
         swl_regex  : constant String := "--regex";
         sws_verb   : constant String := "-v";
         swl_verb   : constant String := "--verbose";
         sws_nocat  : constant String := "-U";
         swl_nocat  : constant String := "--no-repo-update";
         sws_repo   : constant String := "-r";
         swl_repo   : constant String := "--repository";
         error_rec  : constant String := "Unrecognized option: ";
         error_exp  : constant String := "Unexpected argument: ";
         error_chk  : constant String := "Attempt to redefine check action: ";
         error_ann  : constant String := "Attempt to redefine annotation action: ";
         error_PR   : constant String := "The --provides and --requires switches " &
                                         "are mutually exclusive.";
         hyphen     : constant Character := '-';

      begin
         if result.parse_error then
            return;
         end if;

         if last_cmd = nothing_pending then
            case result.command is

               when cv_add =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = "-A" or else datum = "--automatic" then
                     result.add_mark_automatic := True;
                  elsif datum = "-f" or else datum = "--force" then
                     result.add_force_reinstall := True;
                  elsif datum = "-I" or else datum = "--no-scripts" then
                     result.add_skip_scripts := True;
                  elsif datum = "-M" or else datum = "--accept-missing" then
                     result.add_accept_missing := True;
                  else
                     if datum (datum'First) = hyphen then
                        set_error (error_rec & datum);
                     else
                        result.verb_work_queue.Append (datumtxt);
                     end if;
                  end if;

               when cv_alias =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = "-l" or else datum = "--list" then
                     result.alias_list := True;
                  else
                     result.verb_work_queue.Append (datumtxt);
                  end if;

               when cv_annotate =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = "-A" or else datum = "--add" then
                     if result.annot_action = no_annotate_action then
                        result.annot_action := add;
                        if result.verb_all_packages then
                           last_cmd := annotate_tag;
                        end if;
                     else
                        set_error (error_ann & datum);
                     end if;
                  elsif datum = "-M" or else datum = "--modify" then
                     if result.annot_action = no_annotate_action then
                        result.annot_action := modify;
                        if result.verb_all_packages then
                           last_cmd := annotate_tag;
                        end if;
                     else
                        set_error (error_ann & datum);
                     end if;
                  elsif datum = "-D" or else datum = "--delete" then
                     if result.annot_action = no_annotate_action then
                        result.annot_action := delete;
                        if result.verb_all_packages then
                           last_cmd := annotate_tag;
                        end if;
                     else
                        set_error (error_ann & datum);
                     end if;
                  elsif datum = "-S" or else datum = "--show" then
                     if result.annot_action = no_annotate_action then
                        result.annot_action := show;
                        if result.verb_all_packages then
                           last_cmd := annotate_tag;
                        end if;
                     else
                        set_error (error_ann & datum);
                     end if;
                  else
                     --  no more switches expected.
                     --  "-a -S|D tag" is finished, so data here is error
                     --  "-a -A|M tag [value]" lacks [value] (could be std-in?)
                     --  "-S|D [-Cgix] pkg-name tag" needs two trailing
                     --  "-A|M [-Cgix] pkg-name tag [value]" needs three
                     if datum (1) = hyphen then
                        set_error (error_rec & datum);
                     else
                        if result.verb_all_packages then
                           if result.annot_action = show or else
                             result.annot_action = delete
                           then
                              set_error (error_exp & datum);
                           else
                              if IsBlank (result.annot_tag_value) then
                                 result.annot_tag_value := datumtxt;
                              else
                                 set_error (error_exp & datum);
                              end if;
                           end if;
                        else
                           if IsBlank (result.verb_name_pattern) then
                              result.verb_name_pattern := datumtxt;
                           elsif IsBlank (result.annot_tag_name) then
                              result.annot_tag_name := datumtxt;
                           elsif IsBlank (result.annot_tag_value) then
                              result.annot_tag_value := datumtxt;
                           else
                              set_error (error_exp & datum);
                           end if;
                        end if;
                     end if;
                  end if;

               when cv_autoremove =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif datum = sws_dryrun or else datum = swl_dryrun then
                     result.verb_dry_run := True;
                  else
                     set_error (error_rec & datum);
                  end if;

               when cv_backup =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = "-d" or else datum = "--dump" then
                     if IsBlank (result.backup_restore) then
                        last_cmd := backup_destination;
                     else
                        set_error ("dump dest_file already defined, extra: " & datum);
                     end if;
                  elsif datum = "-r" or else datum = "--restore" then
                     if IsBlank (result.backup_dump) then
                        last_cmd := backup_source;
                     else
                        set_error ("restore src_file already defined, extra: " & datum);
                     end if;
                  else
                     set_error (error_rec & datum);
                  end if;

               when cv_check =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif datum = sws_dryrun or else datum = swl_dryrun then
                     result.verb_dry_run := True;
                  elsif datum = sws_verb or else datum = swl_verb then
                     result.verb_verbose := True;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = "-B" or else datum = "--shlibs" then
                     if result.check_action = unset_action then
                        result.check_action := shared_libraries;
                     else
                        set_error (error_chk & datum);
                     end if;
                  elsif datum = "-d" or else datum = "--dependencies" then
                     if result.check_action = unset_action then
                        result.check_action := locate_missing_deps;
                     else
                        set_error (error_chk & datum);
                     end if;
                  elsif datum = "-s" or else datum = "--checksums" then
                     if result.check_action = unset_action then
                        result.check_action := invalid_checksums;
                     else
                        set_error (error_chk & datum);
                     end if;
                  elsif datum = "-r" or else datum = "--recompute" then
                     if result.check_action = unset_action then
                        result.check_action := recompute_checksums;
                     else
                        set_error (error_chk & datum);
                     end if;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                  end if;

               when cv_clean =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif datum = sws_dryrun or else datum = swl_dryrun then
                     result.verb_dry_run := True;
                  elsif datum = sws_all or else datum = swl_all then
                     result.verb_all_packages := True;
                  else
                     set_error (error_rec & datum);
                  end if;

               when cv_config =>
                  if IsBlank (result.config_key) then
                     result.config_key := datumtxt;
                  else
                     set_error ("Only one config key is permitted.");
                  end if;

               when cv_create =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_glob or else datum = swl_glob then
                     result.verb_shell_glob := True;
                  elsif datum = sws_regex or else datum = swl_regex then
                     result.verb_use_regex := True;
                  elsif datum = sws_all or else datum = swl_all then
                     result.verb_all_packages := True;
                  elsif datum = sws_verb or else datum = swl_verb then
                     result.verb_verbose := True;
                  elsif datum = "-m" or else datum = "--metadata" then
                     last_cmd := create_metadatadir;
                  elsif datum = "-M" or else datum = "--manifest" then
                     last_cmd := create_manifest;
                  elsif datum = "-o" or else datum = "--out-dir" then
                     last_cmd := create_outdir;
                  elsif datum = "-r" or else datum = "--root-dir" then
                     last_cmd := create_rootdir;
                  elsif datum = "-p" or else datum = "--plist" then
                     last_cmd := create_plist;
                  elsif datum = "-n" or else datum = "--no-clobber" then
                     result.create_ban_overwrite := True;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                  end if;

               when cv_delete | cv_remove =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = sws_dryrun or else datum = swl_dryrun then
                     result.verb_dry_run := True;
                  elsif datum = "-D" or else datum = "--no-deinstall-script" then
                     result.delete_skip_script := True;
                  elsif datum = "-f" or else datum = "--force" then
                     result.delete_force := True;
                  elsif datum = "-R" or else datum = "--recursive" then
                     result.delete_rev_deps_too := True;
                  else
                     result.verb_work_queue.Append (datumtxt);
                  end if;

               when cv_fetch =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif datum = sws_nocat or else datum = swl_nocat then
                     result.verb_skip_catalog := True;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = "-d" or else datum = "--dependencies" then
                     result.fetch_deps_too := True;
                  elsif datum = "-u" or else datum = "--available-updates" then
                     result.fetch_avail_updates := True;
                  elsif datum = "-o" or else datum = "--output" then
                     last_cmd := fetch_destdir;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                  end if;

               when cv_help =>
                  if result.help_command = cv_unset and then
                    result.help_command2 = cv2_unset
                  then
                     last_cmd := help;
                  else
                     set_error ("The help command only takes one argument");
                  end if;

               when cv_info =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = "-A" or else datum = "--annotations" then
                     result.info_show_annotation := True;
                  elsif datum = "-f" or else datum = "--full" then
                     result.info_full := True;
                  elsif datum = "-R" or else datum = "--raw" then
                     result.info_raw_manifest := True;
                  elsif datum = "-e" or else datum = "--exists" then
                     result.info_alter_return := True;
                  elsif datum = "-D" or else datum = "--pkg-message" then
                     result.info_pkg_message := True;
                  elsif datum = "-I" or else datum = "--comment" then
                     result.info_comment := True;
                  elsif datum = "-d" or else datum = "--dependencies" then
                     result.info_fwd_deps := True;
                  elsif datum = "-r" or else datum = "--required-by" then
                     result.info_rev_deps := True;
                  elsif datum = "-k" or else datum = "--locked" then
                     result.info_lock_status := True;
                  elsif datum = "-l" or else datum = "--list-files" then
                     result.info_list_files := True;
                  elsif datum = "-b" or else datum = "--provided-shlibs" then
                     result.info_shlibs_provided := True;
                  elsif datum = "-B" or else datum = "--required-shlibs" then
                     result.info_shlibs_used := True;
                  elsif datum = "-s" or else datum = "--size" then
                     result.info_total_size := True;
                  elsif datum = "-O" or else datum = "--by-origin" then
                     result.info_search_origin := True;
                  elsif datum = "-p" or else datum = "--prefix" then
                     result.info_install_prefix := True;
                  elsif datum = "--raw-format" then
                     last_cmd := generic_raw_format;
                  elsif datum = "-F" or else datum = "--file" then
                     last_cmd := info_archive_file;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                     if not IsBlank (result.info_file_archive) then
                        set_error ("Use of -F switch with pkg-name is not permitted.");
                     end if;
                  end if;

               when cv_install =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif datum = sws_dryrun or else datum = swl_dryrun then
                     result.verb_dry_run := True;
                  elsif datum = sws_nocat or else datum = swl_nocat then
                     result.verb_skip_catalog := True;
                  elsif aCgix (datum, datumtxt, False) then
                     null;
                  elsif datum = "-A" or else datum = "--automatic" then
                     result.inst_mark_automatic := True;
                  elsif datum = "-f" or else datum = "--force" then
                     result.inst_force_reinstall := True;
                  elsif datum = "-I" or else datum = "--no-install-scripts" then
                     result.inst_skip_scripts := True;
                  elsif datum = "-M" or else datum = "--ignore-missing" then
                     result.inst_ignore_missing := True;
                  elsif datum = "-F" or else datum = "--fetch-only" then
                     result.inst_fetch_only := True;
                  elsif datum = "-R" or else datum = "--recursive" then
                     result.inst_force_rev_deps := True;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                  end if;

               when cv_lock | cv_unlock =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = "-l" or else datum = "--show-locked" then
                     result.lock_show_locked := True;
                  elsif datum = "--has-locked-packages" then
                     result.lock_set_exitcode := True;
                  else
                     if datum (datum'First) = hyphen then
                        set_error (error_rec & datum);
                     else
                        last_cmd := lock_pkgname;
                     end if;
                  end if;

               when cv_query =>
                  if aCgix (datum, datumtxt, False) then
                     null;
                  elsif datum = "-e" or else datum = "--evaluate" then
                     last_cmd := query_condition;
                  elsif datum = "-F" or else datum = "--file" then
                     last_cmd := query_filename;
                  else
                     if IsBlank (result.query_format) then
                        result.query_format := datumtxt;
                     elsif IsBlank (result.verb_name_pattern) then
                        result.verb_name_pattern := datumtxt;
                     else
                        set_error (error_exp & datum);
                     end if;
                  end if;

               when cv_repo =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = "-l" or else datum = "--list-files" then
                     result.repo_list := True;
                  elsif datum = "-m" or else datum = "--meta-file" then
                     last_cmd := repo_meta_File;
                  elsif datum = "-o" or else datum = "--output-dir" then
                     last_cmd := repo_outdir;
                  else
                     if IsBlank (result.repo_path) then
                        result.repo_path := datumtxt;
                     elsif datum = "signing_command:" then
                        last_cmd := repo_signing_cmd;
                     elsif IsBlank (result.repo_rsa_key) then
                        result.repo_rsa_key := datumtxt;
                     else
                        set_error (error_exp & datum);
                     end if;
                  end if;


               when cv_rquery =>
                  if datum = "-e" or else datum = "--evaluate" then
                     last_cmd := rquery_eval_cond;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = "-I" or else datum = "--index-line" then
                     if IsBlank (result.rquery_query_format) then
                        result.rquery_index_line := True;
                     end if;
                  elsif datum = sws_nocat or else datum = swl_nocat then
                     result.verb_skip_catalog := True;
                  else
                     if datum (datum'First) = hyphen then
                        set_error (error_rec & datum);
                     else
                        if not result.rquery_index_line and then
                          IsBlank (result.rquery_query_format)
                        then
                           result.rquery_query_format := datumtxt;
                        else
                           handle_trailing_pkgname (datum, datumtxt);
                        end if;
                     end if;
                  end if;

               when cv_search =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_nocat or else datum = swl_nocat then
                     result.verb_skip_catalog := True;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                  elsif aCgix (datum, datumtxt, use_all => False) then
                     null;
                  elsif datum = "-e" or else datum = "--exact" then
                     result.search_exact_match := True;
                  elsif datum = "-R" or else datum = "--raw" then
                     result.search_raw_manifest := True;
                  elsif datum = "-f" or else datum = "--full" then
                     result.search_modifiers.full := True;
                  elsif datum = "-d" or else datum = "--depends-on" then
                     result.search_modifiers.depends_on := True;
                  elsif datum = "-p" or else datum = "--prefix" then
                     result.search_modifiers.prefix := True;
                  elsif datum = "-s" or else datum = "--size" then
                     result.search_modifiers.size := True;
                  elsif datum = "-Q" or else datum = "--query-modifier" then
                     last_cmd := search_modifier;
                  elsif datum = "-c" or else datum = "--comment" then
                     handle_search_field (search_comment);
                  elsif datum = "-D" or else datum = "--description" then
                     handle_search_field (search_description);
                  elsif datum = "-S" or else datum = "--search" then
                     last_cmd := search_field;
                  elsif datum = "-L" or else datum = "--label" then
                     last_cmd := search_label;
                  elsif datum = "-o" or else datum = "--origins" then
                     handle_label_field (search_origin);
                  elsif datum = "--raw-format" then
                     last_cmd := generic_raw_format;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                  end if;

               when cv_set =>
                  if datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = "-A" or else datum = "--automatic" then
                     last_cmd := set_automatic;
                  elsif datum = "-v" or else datum = "--vital" then
                     last_cmd := set_vital;
                  elsif datum = "-n" or else datum = "--change-name" then
                     last_cmd := set_change_name;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                  end if;

               when cv_shell =>
                  if IsBlank (result.shell_pass_along) then
                     result.shell_pass_along := datumtxt;
                  else
                     SU.Append (result.shell_pass_along, LAT.Vertical_Line & datum);
                  end if;

               when cv_shlib =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = "-P" or else datum = "--provides" then
                     if result.shlib_behavior = undefined_shlib or else
                       result.shlib_behavior = provider
                     then
                        result.shlib_behavior := provider;
                     else
                        set_error (error_PR);
                     end if;
                  elsif datum = "-R" or else datum = "--requires" then
                     if result.shlib_behavior = undefined_shlib or else
                       result.shlib_behavior = consumer
                     then
                        result.shlib_behavior := consumer;
                     else
                        set_error (error_PR);
                     end if;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                  end if;

               when cv_ssh =>
                  set_error (error_exp & datum);

               when cv_stats =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = "-b" or else datum = "--bytes" then
                     result.stats_bytes_only := True;
                  elsif datum = "-l" or else datum = "--local" then
                     if result.stats_behavior = no_database_stats then
                        result.stats_behavior := local_database;
                     elsif result.stats_behavior = remote_database then
                        result.stats_behavior := both_databases;
                     end if;
                  elsif datum = "-r" or else datum = "--remote" then
                     if result.stats_behavior = no_database_stats then
                        result.stats_behavior := remote_database;
                     elsif result.stats_behavior = local_database then
                        result.stats_behavior := both_databases;
                     end if;
                  else
                     set_error (error_rec & datum);
                  end if;

               when cv_update =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                  elsif datum = "-f" or else datum = "--force" then
                     result.update_force := True;
                  else
                     set_error (error_rec & datum);
                  end if;

               when cv_upgrade =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_nocat or else datum = swl_nocat then
                     result.verb_skip_catalog := True;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                  elsif datum = sws_yes or else datum = swl_yes then
                     result.verb_assume_yes := True;
                  elsif datum = sws_dryrun or else datum = swl_dryrun then
                     result.verb_dry_run := True;
                  elsif aCgix (datum, datumtxt, False) then
                     null;
                  elsif datum = "-f" or else datum = "--force" then
                     result.upgrade_force := True;
                  elsif datum = "-I" or else datum = "--no-install-scripts" then
                     result.upgrade_skip_scripts := True;
                  elsif datum = "-F" or else datum = "--fetch-only" then
                     result.upgrade_fetch_only := True;
                  else
                     if datum (datum'First) = hyphen then
                        set_error (error_rec & datum);
                     else
                        result.verb_work_queue.Append (datumtxt);
                     end if;
                  end if;

               when cv_version =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_verb or else datum = swl_verb then
                     result.verb_verbose := True;
                  elsif datum = sws_nocat or else datum = swl_nocat then
                     result.verb_skip_catalog := True;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                  elsif aCgix (datum, datumtxt, False) then
                     null;
                  elsif datum = "-e" or else datum = "--exact" then
                     result.version_exact_match := True;
                  elsif datum = "-l" or else datum = "--like" then
                     last_cmd := version_match_char;
                  elsif datum = "-L" or else datum = "--not-like" then
                     last_cmd := version_not_char;
                  elsif datum = "-o" or else datum = "--origin" then
                     result.version_disp_origin := True;
                  elsif datum = "-O" or else datum = "--match-origin" then
                     last_cmd := version_origin;
                  elsif datum = "-n" or else datum = "--match-name" then
                     last_cmd := version_pkgname;
                  elsif datum = "-t" or else datum = "--test-version" then
                     if result.version_behavior = no_defined_behavior then
                        result.version_behavior := test_versions;
                     else
                        set_error ("The --test-version switch is not compatible with -I, -R, " &
                                     "or -T switches.");
                     end if;
                  elsif datum = "-T" or else datum = "--test-pattern" then
                     if result.version_behavior = no_defined_behavior then
                        result.version_behavior := compare_against_pattern;
                     else
                        set_error ("The --test-pattern switch is not compatible with -I, -R, " &
                                     "or -t switches.");
                     end if;
                  elsif datum = "-R" or else datum = "--remote" then
                     if result.version_behavior = no_defined_behavior then
                        result.version_behavior := use_remote_catalog_state;
                     else
                        set_error ("The -R switch is not compatible with -I, -t, or -T switches.");
                     end if;
                  elsif datum = "-I" or else datum = "--index" then
                     if result.version_behavior = no_defined_behavior then
                        result.version_behavior := use_conspiracy_state;
                     else
                        set_error ("The -I switch is not compatible with -R, -T, or -t switches.");
                     end if;
                  else
                     if result.version_behavior = compare_against_pattern or else
                       result.version_behavior = test_versions
                     then
                        if IsBlank (result.version_test1) then
                           result.version_test1 := datumtxt;
                        elsif IsBlank (result.version_test2) then
                           result.version_test2 := datumtxt;
                        else
                           set_error ("Too many arguments.");
                        end if;
                     else
                        handle_trailing_pkgname (datum, datumtxt);
                     end if;
                  end if;

               when cv_which =>
                  if not IsBlank (result.which_filename) then
                     set_error ("No arguments after filename allowed");
                  elsif datum = sws_quiet or else datum = swl_quiet then
                     result.verb_quiet := True;
                  elsif datum = sws_glob or else datum = swl_glob then
                     result.verb_shell_glob := True;
                  elsif datum = sws_origin or else datum = swl_origin then
                     result.which_origin := True;
                  elsif datum = "-m" or else datum = "--show-match" then
                     result.which_show_match := True;
                  elsif datum = "-p" or else datum = "--path-search" then
                     result.which_path_search := True;
                  else
                     if datum (datum'First) = hyphen then
                        set_error (error_rec & datum);
                     else
                        last_cmd := which_filename;
                     end if;
                  end if;

               when cv_unset =>
                  --  global options
                  if datum = "-d" or else datum = "--debug"
                  then
                     if result.glob_debug < ST_Debug_Level'Last then
                        result.glob_debug := result.glob_debug + 1;
                     end if;
                  elsif datum = "-v" or else datum = "--version"
                  then
                     if result.glob_version < ST_Version'Last then
                        result.glob_version := result.glob_version + 1;
                     end if;
                  elsif datum = "-l" or else datum = "--list"
                  then
                     result.glob_list := True;
                  elsif datum = "-4" or else datum = "--only-ipv4"
                  then
                     result.global_init_flags := init_use_ipv4;
                  elsif datum = "-6" or else datum = "--only-ipv6"
                  then
                     result.global_init_flags := init_use_ipv6;
                  elsif datum = "-N"
                  then
                     result.glob_status_check := True;
                  elsif datum = "-c" or else datum = "--chroot"
                  then
                     last_cmd := global_chroot;
                  elsif datum = "-C" or else datum = "--config"
                  then
                     last_cmd := global_config;
                  elsif datum = "-R" or else datum = "--repo-conf-dir"
                  then
                     last_cmd := global_repoconfdir;
                  elsif datum = "-r" or else datum = "--rootdir"
                  then
                     last_cmd := global_rootdir;
                  elsif datum = "-o" or else datum = "--option"
                  then
                     last_cmd := global_option;
                  elsif datum = "-j" or else datum = "--jail"
                  then
                     last_cmd := global_jail;
                  else
                     --  This far means either we hit a secondary command or
                     --  we've got an unrecognized option
                     result.command := get_command (datum);
                     if result.command = cv_unset then
                        set_error (error_rec & datum);
                     elsif result.command = cv_help then
                        last_cmd := help;
                     end if;

                  end if;
            end case;
         else
            --  insert second part of last seen command
            case last_cmd is
               when nothing_pending    => null;   --  impossible
               when global_chroot      => result.glob_chroot          := datumtxt;
               when global_config      => result.glob_config_file     := datumtxt;
               when global_repoconfdir => result.glob_repo_config_dir := datumtxt;
               when global_rootdir     => result.glob_root_dir        := datumtxt;
               when global_jail        => result.glob_jail            := datumtxt;
               when generic_repo_name  => result.verb_repo_name       := datumtxt;
               when which_filename     => result.which_filename       := datumtxt;
               when lock_pkgname       => result.verb_name_pattern    := datumtxt;
               when backup_source      => result.backup_restore       := datumtxt;
               when backup_destination => result.backup_dump          := datumtxt;
               when annotate_tag       => result.annot_tag_name       := datumtxt;
               when create_metadatadir => result.create_metadata_dir  := datumtxt;
               when create_manifest    => result.create_manifest_file := datumtxt;
               when create_outdir      => result.create_output_dir    := datumtxt;
               when create_rootdir     => result.create_root_dir      := datumtxt;
               when create_plist       => result.create_plist_file    := datumtxt;
               when fetch_destdir      => result.fetch_destdir        := datumtxt;
               when info_archive_file  => result.info_file_archive    := datumtxt;
               when query_filename     => result.query_file_archive   := datumtxt;
               when query_condition    => result.query_eval_condition := datumtxt;
               when repo_signing_cmd   => result.repo_signing_cmd     := datumtxt;
               when repo_outdir        => result.repo_output_dir      := datumtxt;
               when repo_meta_File     => result.repo_meta_file       := datumtxt;
               when rquery_eval_cond   => result.rquery_eval_cond     := datumtxt;
               when set_change_name    => result.set_name_pair        := datumtxt;
               when generic_raw_format => handle_raw_format (datum);
               when search_modifier    => handle_search_modifier (datum);
               when search_field       => handle_search_field (datum);
               when search_label       => handle_label_field (datum);
               when set_automatic      => handle_set_automatic (datum);
               when set_vital          => handle_set_vital (datum);
               when version_match_char => result.version_match_char   := datum (datum'First);
               when version_not_char   => result.version_not_char     := datum (datum'First);
               when version_origin     => result.version_origin       := datumtxt;
               when version_pkgname    => result.version_pkg_name     := datumtxt;
               when global_option      =>
                  if IsBlank (result.glob_option) then
                     result.glob_option := datumtxt;
                  else
                     SU.Append (result.glob_option, LAT.Vertical_Line & datum);
                  end if;
               when help =>
                  result.help_command := get_command (datum);
                  if result.help_command = cv_unset then
                     if datum = progname then
                        result.help_command2 := cv2_main;
                     elsif datum = progname & ".conf" then
                        result.help_command2 := cv2_main_conf;
                     elsif datum = "repository" then
                        result.help_command2 := cv2_repository;
                     else
                        set_error ("'" & datum & "' is not a recognized command");
                     end if;
                  end if;
            end case;
            last_cmd := nothing_pending;
         end if;
      end translate_switch;


      ---------------------------------
      --  check_annotate_stdin
      ---------------------------------
      procedure check_annotate_stdin is
      begin
         if result.annot_action = add or else
           result.annot_action = modify
         then
            if IsBlank (result.annot_tag_value) then
               declare
                  c : Character;
               begin
                  while not TIO.End_Of_File loop
                     TIO.Get (c);
                     if c = ' ' then
                        set_error ("Only one tag expected through standard-in stream");
                     end if;
                     SU.Append (result.annot_tag_value, c);
                  end loop;
               end;
            end if;
         end if;
      end check_annotate_stdin;


      ---------------------------------
      --  check_create_incompatibilities
      ---------------------------------
      procedure check_create_incompatibilities
      is
         msg : constant String := "Switches -a, -g, -x, -m and -M are incompatible";
      begin
         if result.command = cv_create then
            if not IsBlank (result.create_metadata_dir) then
               if result.verb_all_packages or else
                 result.verb_use_regex or else
                 result.verb_shell_glob or else
                 not IsBlank (result.create_manifest_file)
               then
                  set_error (msg);
               end if;
            end if;

            if not IsBlank (result.create_manifest_file) then
               if result.verb_all_packages or else
                 result.verb_use_regex or else
                 result.verb_shell_glob
               then
                  set_error (msg);
               end if;
            end if;

            if result.verb_all_packages then
               if result.verb_shell_glob or else
                 result.verb_use_regex
               then
                  set_error (msg);
               end if;
            end if;

            if result.verb_shell_glob and then result.verb_use_regex
            then
               set_error (msg);
            end if;
         end if;
      end check_create_incompatibilities;

      ---------------------------------
      --  check_implied_info_all
      ---------------------------------
      procedure check_implied_info_all is
      begin
         --  These command imply -a
         --  ravensw info
         --  ravensw info -q
         if result.command = cv_info then
            if not result.verb_all_packages and then
              not result.verb_case_sensitive and then
              not result.verb_case_blind and then
              not result.verb_shell_glob and then
              not result.verb_use_regex and then
              not result.info_show_annotation and then
              not result.info_full and then
              not result.info_raw_manifest and then
              not result.info_alter_return and then
              not result.info_pkg_message and then
              not result.info_comment and then
              not result.info_fwd_deps and then
              not result.info_rev_deps and then
              not result.info_lock_status and then
              not result.info_list_files and then
              not result.info_shlibs_provided and then
              not result.info_shlibs_used and then
              not result.info_total_size and then
              not result.info_search_origin and then
              not result.info_install_prefix and then
              result.verb_raw_format = no_raw_format and then
              IsBlank (result.info_file_archive)
            then
               result.verb_all_packages := True;
            end if;
         end if;
      end check_implied_info_all;

      ---------------------------------
      --  check_implied_query_all
      ---------------------------------
      procedure check_implied_query_all is
      begin
         if result.command = cv_query then
            if not result.verb_all_packages and then
              not result.verb_shell_glob and then
              not result.verb_use_regex and then
              IsBlank (result.query_eval_condition) and then
              IsBlank (result.verb_name_pattern)
            then
               result.verb_all_packages := True;
            end if;
         end if;
      end check_implied_query_all;

      ---------------------------------
      --  check_implied_rquery_all
      ---------------------------------
      procedure check_implied_rquery_all is
      begin
         if result.command = cv_rquery then
            if not result.verb_all_packages and then
              not result.verb_shell_glob and then
              not result.verb_use_regex and then
              IsBlank (result.rquery_eval_cond) and then
              IsBlank (result.verb_name_pattern)
            then
               result.verb_all_packages := True;
            end if;
         end if;
      end check_implied_rquery_all;

      ---------------------------------
      --  check_stats_default
      ---------------------------------
      procedure check_stats_default is
      begin
         if result.stats_behavior = no_database_stats then
            result.stats_behavior := both_databases;
         end if;
      end check_stats_default;

   begin
      expand_command_line;
      expanded_args.Iterate (translate_switch'Access);
      check_annotate_stdin;
      check_create_incompatibilities;
      check_implied_rquery_all;
      check_implied_query_all;
      check_implied_info_all;
      check_stats_default;

      return result;
   end parse_command_line;

end Cmd.Line;
