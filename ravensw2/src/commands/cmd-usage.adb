--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings; use Core.Strings;

package body Cmd.Usage is

   --------------------------------------------------------------------
   --  command_line_valid
   --------------------------------------------------------------------
   function command_line_valid (comline : Cldata) return Boolean
   is
   begin
      case comline.command is
         when cv_unset =>      return no_command_verb (comline);
         when cv_add   =>      return verb_add (comline);
         when cv_alias =>      return verb_alias (comline);
         when cv_annotate =>   return verb_annotate (comline);
         when cv_autoremove => return verb_autoremove (comline);
         when cv_backup =>     return verb_backup (comline);
         when cv_check =>      return verb_check (comline);
         when cv_clean =>      return verb_clean (comline);
         when cv_config =>     return verb_config (comline);
         when cv_create =>     return verb_create (comline);
         when cv_delete =>     return verb_delete (comline);
         when cv_fetch =>      return verb_fetch (comline);
         when cv_help =>       return verb_help (comline);
         when cv_info =>       return verb_info (comline);
         when cv_install =>    return verb_install (comline);
         when cv_lock =>       return verb_lock (comline);
         when cv_query =>      return verb_query (comline);
         when cv_remove =>     return verb_delete (comline);
         when cv_repo =>       return verb_repo (comline);
         when cv_rquery =>     return verb_rquery (comline);
         when cv_search =>     return verb_search (comline);
         when cv_set =>        return verb_set (comline);
         when cv_shell =>      return True;
         when cv_shlib =>      return verb_shlib (comline);
         when cv_ssh =>        return verb_ssh (comline);
         when cv_stats =>      return verb_stats (comline);
         when cv_unlock =>     return verb_unlock (comline);
         when cv_update =>     return verb_update (comline);
         when cv_upgrade =>    return verb_upgrade (comline);
         when cv_version =>    return verb_version (comline);
         when cv_which =>      return verb_which (comline);
      end case;
   end command_line_valid;


   --------------------------------------------------------------------
   --  display_error
   --------------------------------------------------------------------
   procedure display_error (error_msg : String) is
   begin
      if error_msg /= "" then
         TIO.Put_Line (TIO.Standard_Error, progname & ": " & error_msg);
      end if;
   end display_error;


   --------------------------------------------------------------------
   --  display_usage
   --------------------------------------------------------------------
   procedure display_usage (usage_msg : String; first_line : Boolean)
   is
   begin
      if first_line then
         TIO.Put_Line (TIO.Standard_Error, "Usage: " & progname & " " & usage_msg);
      else
         TIO.Put_Line (TIO.Standard_Error, "       " & progname & " " & usage_msg);
      end if;
   end display_usage;


   --------------------------------------------------------------------
   --  insert_carriage_return
   --------------------------------------------------------------------
   procedure insert_carriage_return is
   begin
      TIO.Put_Line (TIO.Standard_Error, "");
   end insert_carriage_return;


   --------------------------------------------------------------------
   --  display_help_suggestion
   --------------------------------------------------------------------
   procedure display_help_suggestion (command : Command_verb)
   is
      main_msg : constant String :=
        "For more information on available commands and options see "
        & SQ (progname & " help") & ".";
   begin
      insert_carriage_return;
      case command is
         when cv_unset => TIO.Put_Line (TIO.Standard_Error, main_msg);
         when others   => TIO.Put_Line
              (TIO.Standard_Error,
               "For more information see " &
                 SQ (progname & " help " & convert_command_enum_to_label (command)) & ".");
      end case;
   end display_help_suggestion;


   --------------------------------------------------------------------
   --  no_command_verb
   --------------------------------------------------------------------
   function no_command_verb (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "[-v] [-d] [-l] [-N] " &
        "[-j <jail name or id>|-c <chroot path>|-r <rootdir>] " &
        "[-C <configuration file>] [-R <repo config dir>] " &
        "[-o var=value] [-4|-6] <command> [<args>]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_unset);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         --  check if no arguments given
         if comline.glob_debug = ST_Debug_Level'First and then
           comline.glob_version = ST_Version'First and then
           not comline.glob_list and then
           not comline.glob_status_check and then
           IsBlank (comline.glob_chroot) and then
           IsBlank (comline.glob_config_file) and then
           IsBlank (comline.glob_repo_config_dir) and then
           IsBlank (comline.glob_root_dir) and then
           IsBlank (comline.glob_option) and then
           IsBlank (comline.glob_jail) and then
           comline.global_init_flags = init_none
         then
            return alert ("Not enough arguments");
         end if;

         --  check if jail requested when unsupported
         if not jail_supported and then not IsBlank (comline.glob_jail) then
            return alert ("Jail operation is not supported on this platform");
         end if;

         --  Only three switches used without a command verb
         if comline.glob_version = ST_Version'First and then
           not comline.glob_status_check and then
           not comline.glob_list
         then
            return alert ("No commands specified");
         end if;

         return True;
      end if;
   end no_command_verb;


   --------------------------------------------------------------------
   --  verb_add
   --------------------------------------------------------------------
   function verb_add (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "add [-IAfqM] <pkg-name> ...";
         msg2 : constant String := "add [-IAfqM] <protocol>://<path>/<pkg-name> ...";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_add);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if comline.verb_work_queue.Is_Empty then
            return alert ("Missing <pkg-name> ...");
         end if;

         return True;
      end if;
   end verb_add;


   --------------------------------------------------------------------
   --  verb_alias
   --------------------------------------------------------------------
   function verb_alias (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "alias [-ql] [alias]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_alias);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         return True;
      end if;
   end verb_alias;


   --------------------------------------------------------------------
   --  verb_annotate
   --------------------------------------------------------------------
   function verb_annotate (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "annotate [-Cgiqxy] [-A|M] <pkg-name> <tag> [<value>]";
         msg2 : constant String := "annotate [-Cgiqxy] [-S|D] <pkg-name> <tag>";
         msg3 : constant String := "annotate [-qy] -a [-A|M] <tag> [<value>]";
         msg4 : constant String := "annotate [-qy] -a [-S|D] <tag>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_usage (msg4, False);
         display_help_suggestion (cv_annotate);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if comline.annot_action = no_annotate_action then
            return alert ("Required switch A|M|S|D missing");
         end if;
         if IsBlank (comline.annot_tag_name) then
            return alert ("Required <tag> missing");
         end if;
         if not comline.verb_all_packages and then IsBlank (comline.verb_name_pattern) then
            return alert ("Required <pkg-name> missing");
         end if;

         return True;
      end if;
   end verb_annotate;


   --------------------------------------------------------------------
   --  verb_autoremove
   --------------------------------------------------------------------
   function verb_autoremove (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "autoremove [-ynq]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_autoremove);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         return True;
      end if;
   end verb_autoremove;


   --------------------------------------------------------------------
   --  verb_backup
   --------------------------------------------------------------------
   function verb_backup (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "backup [-q] -d <dest_file>";
         msg2 : constant String := "backup [-q] -r <src_file>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_backup);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not IsBlank (comline.backup_dump) and then not IsBlank (comline.backup_restore) then
            return alert ("The -d and -r switches are mutually exclusive");
         end if;
         if IsBlank (comline.backup_dump) and then IsBlank (comline.backup_restore) then
            return alert ("Either a backup or a restore operation must be requested");
         end if;

         return True;
      end if;
   end verb_backup;


   --------------------------------------------------------------------
   --  verb_check
   --------------------------------------------------------------------
   function verb_check (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "check -B|-d|-s|-r [-qvy] -a";
         msg2 : constant String := "check -B|-d|-s|-r [-qvy] [-Cgix] <pattern>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_check);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if comline.check_action = unset_action then
            return alert ("Exactly one of B|d|s|r switches is required");
         end if;

         if not comline.verb_all_packages and then IsBlank (comline.verb_name_pattern) then
            return alert ("Missing <pattern>");
         end if;

         return True;
      end if;
   end verb_check;


   --------------------------------------------------------------------
   --  verb_clean
   --------------------------------------------------------------------
   function verb_clean (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "clean [-anqy]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_clean);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         return True;
      end if;
   end verb_clean;


   --------------------------------------------------------------------
   --  verb_config
   --------------------------------------------------------------------
   function verb_config (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "config <name>";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_config);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if IsBlank (comline.config_key) then
            return alert ("Missing <name>");
         end if;

         return True;
      end if;
   end verb_config;


   --------------------------------------------------------------------
   --  verb_create
   --------------------------------------------------------------------
   function verb_create (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "create [-Ohnqv] [-o outdir] [-p plist] [-r rootdir] " &
                                   "-m metadatadir";
         msg2 : constant String := "create [-Ohnqv] [-o outdir] [-r rootdir] -M manifest";
         msg3 : constant String := "create [-Ohgnqvx] [-o outdir] [-r rootdir] pkg-name ...";
         msg4 : constant String := "create [-Ohnqv] [-o outdir] [-r rootdir] -a";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_usage (msg4, False);
         display_help_suggestion (cv_create);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not comline.verb_all_packages and then
           IsBlank (comline.create_metadata_dir) and then
           IsBlank (comline.create_manifest_file) and then
           IsBlank (comline.verb_name_pattern)
         then
            return alert ("Either -m, -M, or pkg-name is required.");
         end if;

         if IsBlank (comline.create_metadata_dir) and then
           IsBlank (comline.create_manifest_file) and then
           not IsBlank (comline.create_root_dir)
         then
            return alert ("Do not specify a rootdir without also specifying " &
                            "either a metadatadir or manifest");
         end if;

         return True;
      end if;
   end verb_create;


   --------------------------------------------------------------------
   --  verb_delete
   --------------------------------------------------------------------
   function verb_delete (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "delete [-DfnqRy] [-Cgix] <pkg-name> ...";
         msg2 : constant String := "delete [-Dnqy] -a";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_delete);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not comline.verb_all_packages and then comline.verb_work_queue.Is_Empty then
            return alert ("Missing <pkg-name> ...");
         end if;
         return True;
      end if;
   end verb_delete;


   --------------------------------------------------------------------
   --  verb_fetch
   --------------------------------------------------------------------
   function verb_fetch (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "fetch [-r reponame] [-o destdir] [-dqUy] [-Cgix] " &
                                   "<pkg-name> <...>";
         msg2 : constant String := "fetch [-r reponame] [-dqUy] -a";
         msg3 : constant String := "fetch [-r reponame] [-dqUy] -u";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_help_suggestion (cv_fetch);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if comline.verb_all_packages and then comline.fetch_avail_updates then
            return alert ("The -a and -u options are mutually exclusive.");
         end if;

         if not comline.verb_all_packages and then
           not comline.fetch_avail_updates and then
           comline.verb_work_queue.Is_Empty
         then
            return alert ("Missing <pkg-name> ...");
         end if;

         return True;
      end if;
   end verb_fetch;


   --------------------------------------------------------------------
   --  verb_help
   --------------------------------------------------------------------
   function verb_help (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "help <command>";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         return True;
      end if;
   end verb_help;


   --------------------------------------------------------------------
   --  verb_info
   --------------------------------------------------------------------
   function verb_info (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "info <pkg-name>";
         msg2 : constant String := "info -a";
         msg3 : constant String := "info [-AbBDdefIklOqRrs] [-Cgix] <pkg-name>";
         msg4 : constant String := "info [-AbBDdfIlqRrs] -F <pkg-file>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_usage (msg4, False);
         display_help_suggestion (cv_info);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not IsBlank (comline.verb_name_pattern) then
            if comline.verb_all_packages or else not IsBlank (comline.info_file_archive) then
               return alert ("<pkg-name> not used with -a or -F switch");
            end if;
         end if;

         if not comline.verb_all_packages and then
           IsBlank (comline.verb_name_pattern)
         then
            return alert ("Missing <pkg-name>");
         end if;

         return True;
      end if;
   end verb_info;


   --------------------------------------------------------------------
   --  verb_install
   --------------------------------------------------------------------
   function verb_install (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "install [-AfInFMqRUy] [-r reponame] [-Cgix] <pkg-name> ...";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_install);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if comline.verb_work_queue.Is_Empty then
            return alert ("Missing <pkg-name> ...");
         end if;

         return True;
      end if;
   end verb_install;


   --------------------------------------------------------------------
   --  verb_lock
   --------------------------------------------------------------------
   function verb_lock (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "lock [-lqy] [-a|[-Cgix] <pkg-name>]";
         msg2 : constant String := "lock [-lqy] [-a|[-Cgix] <pkg-name>]";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_lock);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not comline.lock_show_locked and then
           not comline.verb_all_packages and then
           IsBlank (comline.verb_name_pattern)
         then
            return alert ("Missing <pkg-name>");
         end if;

         return True;
      end if;
   end verb_lock;


   --------------------------------------------------------------------
   --  verb_unlock
   --------------------------------------------------------------------
   function verb_unlock (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "unlock [-lqy] [-a|[-Cgix] <pkg-name>]";
         msg2 : constant String := "unlock [-lqy] [-a|[-Cgix] <pkg-name>]";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_unlock);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not comline.lock_show_locked and then
           not comline.verb_all_packages and then
           IsBlank (comline.verb_name_pattern)
         then
            return alert ("Missing <pkg-name>");
         end if;

         return True;
      end if;
   end verb_unlock;


   --------------------------------------------------------------------
   --  verb_stats
   --------------------------------------------------------------------
   function verb_stats (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "stats [-qlrb]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_stats);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         return True;
      end if;
   end verb_stats;


   --------------------------------------------------------------------
   --  verb_ssh
   --------------------------------------------------------------------
   function verb_ssh (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "ssh";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_ssh);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         return True;
      end if;
   end verb_ssh;


   --------------------------------------------------------------------
   --  verb_shlib
   --------------------------------------------------------------------
   function verb_shlib (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "shlib [-q] [-P|R] <library>";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         TIO.Put_Line (TIO.Standard_Error, "<library> should be a filename without leading path.");
         display_help_suggestion (cv_shlib);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if contains (comline.verb_name_pattern, " ") or else
           contains (comline.verb_name_pattern, "/")
         then
            return alert ("Library contains a space or forward slash");
         end if;

         return True;
      end if;
   end verb_shlib;


   --------------------------------------------------------------------
   --  verb_set
   --------------------------------------------------------------------
   function verb_set (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "set [-a] [-A [01]] [-n <oldname>,<newname>] " &
           "[-y] [-Cgix] [-v 0|1] <pkg-name>";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_set);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not contains (comline.set_name_pair, ",") then
            return alert ("Wrong format for -n. Expecting oldname,newname, got: " &
                            USS (comline.set_name_pair));
         end if;

         if comline.set_automatic_flag = undefined_flag and then
           comline.set_vital_flag = undefined_flag and then
           IsBlank (comline.set_name_pair)
         then
            return alert ("One of -A, -n, and -v must be used.");
         end if;

         if not comline.verb_all_packages and then
           IsBlank (comline.verb_name_pattern)
         then
            return alert ("Missing <pkg-name>");
         end if;

         return True;
      end if;
   end verb_set;


   --------------------------------------------------------------------
   --  verb_search
   --------------------------------------------------------------------
   function verb_search (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "search [-eU] [-r repo] [-S search] [-L label] " &
                                   "[-Q mod]... [-Cgix] <pkg-name>";
         msg2 : constant String := "search [-cDdefopqRU] [-r repo] [-Cgix] <pattern>";
         msg3 : constant String := "            ";
         msg4 : constant String := "       Search and Label options:";
         msg5 : constant String := "       Output Modifiers:";
         max  : constant Natural := 75;
         n    : Natural := msg3'Length;
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         insert_carriage_return;

         --  List search and label fields
         TIO.Put (TIO.Standard_Error, msg4);
         for opt in A_Search_Field'Range loop
            if opt /= A_Search_Field'First then
               declare
                  item : constant String := " " & convert_search_enum_to_label (opt);
               begin
                  if n + item'Length > max then
                     insert_carriage_return;
                     TIO.Put (TIO.Standard_Error, msg3);
                     n := msg3'Length;
                  end if;
                  n := n + item'Length;
                  TIO.Put (TIO.Standard_Error, item);
               end;
            end if;
         end loop;
         insert_carriage_return;

         --  List search modifiers
         TIO.Put (TIO.Standard_Error, msg5);
         n := msg5'Length;
         for opt in ST_Modifier_Index'Range loop
            declare
               item : constant String := " " & convert_modifier_enum_to_label (opt);
            begin
               if n + item'Length > max then
                  insert_carriage_return;
                  TIO.Put (TIO.Standard_Error, msg3);
                  n := msg3'Length;
               end if;
               n := n + item'Length;
               TIO.Put (TIO.Standard_Error, item);
            end;
         end loop;
         insert_carriage_return;

         --  Finally suggest help search
         display_help_suggestion (cv_search);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if IsBlank (comline.verb_name_pattern)
         then
            return alert ("Missing <pattern> or <pkg-name>");
         end if;

         return True;
      end if;
   end verb_search;


   --------------------------------------------------------------------
   --  verb_repo
   --------------------------------------------------------------------
   function verb_repo (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "repo [-lqL] [-o output-dir] <repo-path> " &
           "[<rsa-key>|signing_command: <the command>]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_repo);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if IsBlank (comline.repo_path) then
            return alert ("Missing <repo-path>");
         end if;

         return True;
      end if;
   end verb_repo;


   --------------------------------------------------------------------
   --  verb_which
   --------------------------------------------------------------------
   function verb_which (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "which [-mqgop] <file>";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_which);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if IsBlank (comline.which_filename) then
            return alert ("missing <file>");
         end if;
         return True;
      end if;
   end verb_which;


   --------------------------------------------------------------------
   --  verb_upgrade
   --------------------------------------------------------------------
   function verb_upgrade (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "upgrade [-fInFqUy] [-r reponame] [-Cgix] <pkg-name> ...";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_upgrade);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         return True;
      end if;
   end verb_upgrade;


   --------------------------------------------------------------------
   --  verb_update
   --------------------------------------------------------------------
   function verb_update (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "update [-fq] [-r reponame]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_update);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         return True;
      end if;
   end verb_update;


   --------------------------------------------------------------------
   --  verb_version
   --------------------------------------------------------------------
   function verb_version (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      procedure print_usage;

      procedure print_usage
      is
         msg1 : String := "version [-IPR] [-hoqvU] [-l limchar] [-L limchar] [-Cegix pattern]";
         msg2 : String := "        [-r reponame] [-O origin|-n pkgname] [index]";
         msg3 : String := "version -t <version1> <version2>";
         msg4 : String := "version -T <pkgname> <pattern>";
      begin
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_usage (msg4, False);
      end print_usage;

      function alert (error_msg : String) return Boolean is
      begin
         display_error (error_msg);
         print_usage;
         display_help_suggestion (cv_version);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         case comline.version_match_char is
            when Character'First => null;
            when '<' | '>' | '=' | '?' | '!' => null;
            when others => return alert ("Illegal character for -l switch");
         end case;

         case comline.version_not_char is
            when Character'First => null;
            when '<' | '>' | '=' | '?' | '!' => null;
            when others => return alert ("Illegal character for -L switch");
         end case;

         if comline.version_behavior = test_versions then
            if IsBlank (comline.version_test1) or else
              IsBlank (comline.version_test2)
            then
               return alert ("--test-version requires 2 arguments");
            end if;
         end if;

         if comline.version_behavior = compare_against_pattern then
            if IsBlank (comline.version_test1) or else
              IsBlank (comline.version_test2)
            then
               return alert ("--test-pattern requires 2 arguments");
            end if;
         end if;

         if comline.verb_help then
            print_usage;
            display_help_suggestion (cv_version);
         end if;

         return True;
      end if;
   end verb_version;


   --------------------------------------------------------------------
   --  verb_query
   --------------------------------------------------------------------
   function verb_query (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "query <query-format> <pkg-name>";
         msg2 : constant String := "query [-a] <query-format>";
         msg3 : constant String := "query -F <pkg-name> <query-format>";
         msg4 : constant String := "query -e <evaluation> <query-format>";
         msg5 : constant String := "query [-Cgix] <query-format> <pattern> <...>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_usage (msg4, False);
         display_usage (msg5, False);
         display_help_suggestion (cv_query);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if IsBlank (comline.query_format) then
            return alert ("Missing <query-format>");
         end if;

         return True;
      end if;
   end verb_query;


   --------------------------------------------------------------------
   --  verb_rquery
   --------------------------------------------------------------------
   function verb_rquery (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "rquery [-r reponame] -I|<query-format> <pkg-name>";
         msg2 : constant String := "rquery [-a] [-r reponame] -I|<query-format>";
         msg3 : constant String := "rquery -e <evaluation> [-r reponame] -I|<query-format>";
         msg4 : constant String := "rquery [-Cgix] [-r reponame] -I|<query-format> " &
                                   "<pattern> <...>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_usage (msg4, False);
         display_help_suggestion (cv_query);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not comline.verb_all_packages and then
           IsBlank (comline.verb_name_pattern)
         then
            return alert ("Missing <pkg-name>");
         end if;
         if not comline.rquery_index_line and then
           IsBlank (comline.rquery_query_format)
         then
            return alert ("Missing -I or <query-format> (mutually exclusive)");
         end if;

         return True;
      end if;
   end verb_rquery;

end Cmd.Usage;
