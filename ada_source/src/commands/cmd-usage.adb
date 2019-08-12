--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Core.Strings; use Core.Strings;

package body Cmd.Usage is

   package TIO renames Ada.Text_IO;

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
         when cv_shell =>      return True;
         when cv_shlib =>      return verb_shlib (comline);
         when cv_ssh =>        return verb_ssh (comline);
         when cv_stats =>      return verb_stats (comline);
         when cv_unlock =>     return verb_unlock (comline);
         when cv_set =>        return verb_set (comline);
           when others => return True;
      end case;
   end command_line_valid;


   --------------------------------------------------------------------
   --  display_error
   --------------------------------------------------------------------
   procedure display_error (error_msg: String) is
   begin
      TIO.Put_Line (progname & ": " & error_msg);
   end display_error;


   --------------------------------------------------------------------
   --  display_usage
   --------------------------------------------------------------------
   procedure display_usage (usage_msg : String; first_line : Boolean)
   is
   begin
      if first_line then
         TIO.Put_Line ("Usage: " & progname & " " & usage_msg);
      else
         TIO.Put_Line ("       " & progname & " " & usage_msg);
      end if;
   end display_usage;


   --------------------------------------------------------------------
   --  display_help_suggestion
   --------------------------------------------------------------------
   procedure display_help_suggestion (command : Command_verb)
   is
      main_msg : constant String := "For more information on available commands and " &
        "options see '" & progname & " help'.";
   begin
      TIO.Put_Line ("");
      case command is
         when cv_unset => TIO.Put_Line (main_msg);
         when others   => TIO.Put_Line ("For more information see '" & progname & " help " &
                                       convert_command_enum_to_label (command) & "'.");
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

         --  Only two switches used without a command verb
         if comline.glob_version = ST_Version'First and then
           not comline.glob_status_check
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
         if not IsBlank (comline.backup_dump) and not IsBlank (comline.backup_restore) then
            return alert ("The -d and -r switches are mutually exclusive");
         end if;
         if IsBlank (comline.backup_dump) and IsBlank (comline.backup_restore) then
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
         if not comline.verb_all_packages and comline.verb_work_queue.Is_Empty then
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
         display_usage (msg2, True);
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
         display_usage (msg2, True);
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
         TIO.Put_Line ("<library> should be a filename without leading path.");
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
           isblank (comline.verb_name_pattern)
         then
            return alert ("Missing <pkg-name>");
         end if;

         return True;
      end if;
   end verb_set;

end Cmd.Usage;
