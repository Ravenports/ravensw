--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Ada.Directories;
with GNAT.OS_Lib;

with Core.Strings; use Core.Strings;

package body Cmd.Help is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package OSL renames GNAT.OS_Lib;

   --------------------------------------------------------------------
   --  execute_help_command
   --------------------------------------------------------------------
   function execute_help_command (comline : Cldata) return Boolean
   is
      manprefix : constant String := install_prefix & "/share/man";
   begin
      --  There's no man page ravensw-help.8.  Trying ravenadm help help will result
      --  in a manpage not found error.
      if comline.help_command = cv_unset or else
        comline.help_command = cv_help
      then
         print_global_options;
         print_command_summary;
         TIO.Put_Line ("");
         TIO.Put_Line ("For more information on the different commands see '" & progname &
                         " help <command>'.");
         return True;
      else
         case comline.help_command2 is
            when cv2_unset =>
               return show_man_page
                 (manprefix & "/man8/ravensw-" &
                    convert_command_enum_to_label (comline.help_command) & ".8.gz");
            when cv2_main =>
               return show_man_page (manprefix & "/man8/ravensw.8.gz");
            when cv2_main_conf =>
               return show_man_page (manprefix & "/man5/ravensw-ravensw.conf.5.gz");
            when cv2_repository =>
               return show_man_page (manprefix & "/man5/ravensw-repository.5.gz");
         end case;
      end if;
   end execute_help_command;


   --------------------------------------------------------------------
   --  print_global_options
   --------------------------------------------------------------------
   procedure print_global_options
   is
      prog : constant String := progname & "(8) ";
   begin
      TIO.Put_Line ("Global options supported:");
      PL ("-d", "Increment debug level");
      PL ("-j", "Execute " & prog & "inside a jail");
      PL ("-r", "Execute " & prog & "using relocating installation to <rootdir>");
      PL ("-c", "Execute " & prog & "inside a chroot");
      PL ("-C", "Use the specified configuration file");
      PL ("-R", "Directory to search for individual repository configurations");
      PL ("-l", "List available commands and exit");
      PL ("-v", "Display " & prog & "version");
      PL ("-N", "Test if " & prog & "is activated and avoid auto-activation");
      PL ("-o", "Override configuration option from the command line");
      PL ("-4", "Only use IPv4");
      PL ("-6", "Only use IPv6");
   end print_global_options;


   --------------------------------------------------------------------
   --  print_command_summary
   --------------------------------------------------------------------
   procedure print_command_summary is
   begin
      TIO.Put_Line ("");
      TIO.Put_Line ("Commands supported:");
      for command in Command_verb'Range loop
         declare
            C : constant String := convert_command_enum_to_label (command);
         begin
            case command is
               when cv_unset   => null;
               when cv_add     => PL (C, "Compatibility interface to install a package");
               when cv_alias   => PL (C, "List the command line aliases");
               when cv_backup  => PL (C, "Backs-up and restores the local package database");
               when cv_clean   => PL (C, "Cleans old packages from the cache");
               when cv_config  => PL (C, "Display the value of the configuration options");
               when cv_create  => PL (C, "Creates software package distributions");
               when cv_delete  |
                    cv_remove  => PL (C, "Deletes packages from the database and the system");
               when cv_fetch   => PL (C, "Fetches packages from a remote repository");
               when cv_help    => PL (C, "Displays help information");
               when cv_info    => PL (C, "Displays information about installed packages");
               when cv_lock    => PL (C, "Locks package against modifications or deletion");
               when cv_query   => PL (C, "Queries information about installed packages");
               when cv_repo    => PL (C, "Creates a package repository catalog");
               when cv_rquery  => PL (C, "Queries information in repository catalogs");
               when cv_search  => PL (C, "Performs a search of package repository catalogs");
               when cv_ssh     => PL (C, "Package server (to be used via ssh)");
               when cv_stats   => PL (C, "Displays package database statistics");
               when cv_update  => PL (C, "Updates package repository catalogs");
               when cv_upgrade => PL (C, "Performs upgrades of packaged software distributions");
               when cv_unlock  => PL (C, "Unlocks a package, allowing modification or deletion");
               when cv_version => PL (C, "Displays the versions of installed packages");
               when cv_which   => PL (C, "Displays which package installed a specific file");
               when cv_shell   => PL (C, "Opens a debug shell");
               when cv_autoremove =>
                  PL (C, "Removes orphan packages");
               when cv_shlib =>
                  PL (C, "Displays which packages link against a specific shared library");
               when cv_set =>
                  PL (C, "Modifies information about packages in the local database");
               when cv_install =>
                  PL (C, "Installs packages from remote package repositories and local archives");
               when cv_check =>
                  PL (C, "Checks for missing dependencies and database consistency");
               when cv_annotate =>
                  PL (C, "Add, modify or delete tag-value style annotations on packages");
            end case;
         end;
      end loop;
   end print_command_summary;


   --------------------------------------------------------------------
   --  PL
   --------------------------------------------------------------------
   procedure PL (name, value : String)
   is
      width     : constant Natural := 14;
      namespace : String (1 .. width + 1) := (others => ' ');
      namelen   : Natural := name'Length;
   begin
      if namelen > width then
         namelen := width;
      end if;
      namespace (1 .. namelen) := name (name'First .. name'First + namelen - 1);
      TIO.Put_Line (LAT.HT & namespace & value);
   end PL;


   --------------------------------------------------------------------
   --  show_man_page
   --------------------------------------------------------------------
   function show_man_page (manpage : String) return Boolean
   is
      function manprog return String;

      mandoc     : constant String := install_prefix & "/bin/man";
      sysman     : constant String := "/usr/bin/man";
      use_mandoc : Boolean := False;

      function manprog return String is
      begin
         if use_mandoc then
            return mandoc;
         else
            return sysman;
         end if;
      end manprog;

   begin
      if DIR.Exists (mandoc) then
         use_mandoc := True;
      elsif not DIR.Exists (sysman) then
         TIO.Put_Line (TIO.Standard_Error, progname & ": No man program found");
         return False;
      end if;
      if not DIR.Exists (manpage) then
         TIO.Put_Line (TIO.Standard_Error, progname & ": missing manpage: " & manpage);
         return False;
      end if;

      declare
         Result    : Integer;
         Arguments : OSL.Argument_List := (1 => new String'(manprog),
                                           2 => new String'(manpage));
      begin
         OSL.Spawn (Program_Name           => manprog,
                    Args                   => Arguments,
                    Output_File_Descriptor => OSL.Standout,
                    Return_Code            => Result,
                    Err_To_Out             => True);
         for Index in Arguments'Range loop
            OSL.Free (Arguments (Index)); -- Free the argument list
         end loop;
         return (Result = 0);
      end;

   end show_man_page;

end Cmd.Help;
