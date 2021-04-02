--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Cmd.Usage is

   --  Check if error occurred during command line parsing
   --  If not, return true.   If so, show error and dynamic usage.
   function command_line_valid (comline : Cldata) return Boolean;

private

   --  Common routine to display error message (before usage).
   procedure display_error (error_msg : String);

   --  Common routine to display usage messages
   procedure display_usage (usage_msg : String; first_line : Boolean);

   --  Common routine to display usage message without progname
   procedure display_usage_multiline (usage_msg : String);

   --  Common routine to display final help suggestion
   procedure display_help_suggestion (command : Command_verb);

   --  Adds a carriage return to standard error stream
   procedure insert_carriage_return;

   --  Break each command into individual routines.  Any additional
   --  validation checks (if no parsing error exists) can be done here.
   function no_command_verb (comline : Cldata) return Boolean;
   function verb_add (comline : Cldata) return Boolean;
   function verb_alias (comline : Cldata) return Boolean;
   function verb_annotate (comline : Cldata) return Boolean;
   function verb_autoremove (comline : Cldata) return Boolean;
   function verb_backup (comline : Cldata) return Boolean;
   function verb_check (comline : Cldata) return Boolean;
   function verb_clean (comline : Cldata) return Boolean;
   function verb_config (comline : Cldata) return Boolean;
   function verb_create (comline : Cldata) return Boolean;
   function verb_delete (comline : Cldata) return Boolean;
   function verb_fetch (comline : Cldata) return Boolean;
   function verb_help (comline : Cldata) return Boolean;
   function verb_info (comline : Cldata) return Boolean;
   function verb_install (comline : Cldata) return Boolean;
   function verb_lock (comline : Cldata) return Boolean;
   function verb_unlock (comline : Cldata) return Boolean;
   function verb_query (comline : Cldata) return Boolean;
   function verb_repo (comline : Cldata) return Boolean;
   function verb_rquery (comline : Cldata) return Boolean;
   function verb_search (comline : Cldata) return Boolean;
   function verb_set (comline : Cldata) return Boolean;
   function verb_shlib (comline : Cldata) return Boolean;
   function verb_ssh (comline : Cldata) return Boolean;
   function verb_stats (comline : Cldata) return Boolean;
   function verb_update (comline : Cldata) return Boolean;
   function verb_upgrade (comline : Cldata) return Boolean;
   function verb_version (comline : Cldata) return Boolean;
   function verb_which (comline : Cldata) return Boolean;

end Cmd.Usage;
