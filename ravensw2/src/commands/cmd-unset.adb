--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Core.Strings; use Core.Strings;
with Core.Repo;
with Core.Config.Read;
with Core.Status;

package body Cmd.Unset is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  execute_no_command
   --------------------------------------------------------------------
   function execute_no_command (comline : Cldata) return Boolean is
   begin
      --  switch -v
      if comline.glob_version = 1 then
         return basic_version_info;
      end if;

      --  switch -l
      if comline.glob_list then
         return list_available_commands;
      end if;

      if not Initialize_ravensw (comline) then
         return False;
      end if;

      --  switch -vv
      if comline.glob_version = 2 then
         return extended_version_info;
      end if;

      if comline.glob_status_check then
         return do_activation_test;
      end if;

      return True;
   end execute_no_command;


   --------------------------------------------------------------------
   --  Initialize_ravensw
   --------------------------------------------------------------------
   function Initialize_ravensw (comline : Cldata) return Boolean is
   begin
      return Config.Read.establish_configuration
        (path     => USS (comline.glob_config_file),
         reposdir => USS (comline.glob_repo_config_dir),
         flags    => comline.global_init_flags,
         dlevel   => comline.glob_debug,
         options  => USS (comline.glob_option)) = RESULT_OK;
   end Initialize_ravensw;


   --------------------------------------------------------------------
   --  basic_version_info
   --------------------------------------------------------------------
   function basic_version_info return Boolean is
   begin
      TIO.Put_Line (progversion);
      return True;
   end basic_version_info;


   --------------------------------------------------------------------
   --  extended_version_info
   --------------------------------------------------------------------
   function extended_version_info return Boolean is
   begin
      --  Don't pad with 24 characters like FreeBSD pkg, it looks weird.
      --  It must be a holdover from a previous design (pre-ucl?)
      TIO.Put_Line ("Version: " & progversion);
      TIO.Put_Line (Config.Read.config_dump);

      show_repository_info;

      return True;
   end extended_version_info;


   --------------------------------------------------------------------
   --  format_extconfig
   --------------------------------------------------------------------
   function format_extconfig (name, value : String; quotes, last : Boolean) return String
   is
      width     : constant Natural := 16;
      namespace : String (1 .. width) := (others => ' ');
      namelen   : Natural := name'Length;
   begin
      if namelen > width then
         namelen := width;
      end if;
      namespace (1 .. namelen) := name (name'First .. name'First + namelen - 1);
      if quotes then
         if last then
            return "    " & namespace & ": " & LAT.Quotation & value & LAT.Quotation;
         else
            return "    " & namespace & ": " & LAT.Quotation & value & LAT.Quotation & LAT.Comma;
         end if;
      else
         if last then
            return "    " & namespace & ": " & value;
         else
            return "    " & namespace & ": " & value & LAT.Comma;
         end if;
      end if;
   end format_extconfig;


   --------------------------------------------------------------------
   --  print_extconfig
   --------------------------------------------------------------------
   procedure print_extconfig (name, value : String; quotes : Boolean; last : Boolean := False) is
   begin
      TIO.Put_Line (format_extconfig (name, value, quotes, last));
   end print_extconfig;


   --------------------------------------------------------------------
   --  list_available_comands
   --------------------------------------------------------------------
   function list_available_commands return Boolean
   is
      type cols is range 1 .. 5;

      numwords    : constant Natural := Command_verb'Range_Length - 1;
      minlength   : constant Natural := numwords / cols'Range_Length;
      cols_minus1 : constant Natural := numwords mod cols'Range_Length;

      col_length  : array (cols) of Natural := (others => minlength);
      print_order : array (1 .. numwords) of Command_verb;

      column : cols := cols'First;
   begin
      --  Loop will not execute when total commands - 1 is divisible by 5
      --  Currently at this point, uncomment when new commands are added
--        for N in 1 .. cols_minus1 loop
--           col_length (cols (N)) := minlength + 1;
--        end loop;

      declare
         po_index : Natural := 1;  --  Zero-indexed, but we want to skip the first command
      begin
         for N in cols'Range loop
            declare
               index : Positive := Positive (N);
            begin
               for Q in 1 .. col_length (N) loop
                  print_order (index) := Command_verb'Val (po_index);
                  po_index := po_index + 1;
                  index := index + Positive (cols'Last);
               end loop;
            end;
         end loop;
      end;

      for cindex in print_order'Range loop
         declare
            command : Command_verb := print_order (cindex);
            C : constant String := convert_command_enum_to_label (command);
         begin
            case command is
               when cv_unset => null;
               when others =>
                  TIO.Put (pad_right (C, 15));
                  if column = cols'Last then
                     column := cols'First;
                     TIO.Put_Line ("");
                  else
                     column := column + 1;
                  end if;
            end case;
         end;
      end loop;
      if column > cols'First then
         TIO.Put_Line ("");
      end if;
      return True;
   end list_available_commands;


   --------------------------------------------------------------------
   --  show_repository_info
   --------------------------------------------------------------------
   procedure show_repository_info
   is
      joined_list : constant String := Repo.joined_priority_order;
      num_repos   : Natural;
   begin
      TIO.Put_Line ("Repositories:");
      if not IsBlank (joined_list) then
         num_repos := count_char (joined_list, LAT.LF) + 1;
         for x in 1 .. num_repos loop
            declare
               reponame : constant String := specific_field (joined_list, x, LAT.LF & "");
               R        : Repo.A_repo := repo.get_repository (reponame);
            begin
               TIO.Put_Line ("  " & Repo.repo_name (R) & ": {");
               print_extconfig ("url", Repo.repo_url (R), True);
               print_extconfig ("enabled", Repo.repo_enabled (R), False);
               case Repo.repo_mirror_type (R) is
                  when Repo.NOMIRROR => null;
                  when Repo.HTTP | Repo.SRV =>
                     print_extconfig ("mirror_type", Repo.repo_mirror_type (R), True);
               end case;
               case Repo.repo_signature_type (R) is
                  when Repo.SIG_NONE => null;
                  when Repo.SIG_PUBKEY | Repo.SIG_FINGERPRINT =>
                     print_extconfig ("signature_type", Repo.repo_signature_type (R), True);
               end case;
               declare
                  setting : constant String := Repo.repo_fingerprints (R);
               begin
                  if not IsBlank (setting) then
                     print_extconfig ("fingerprints", setting, True);
                  end if;
               end;
               declare
                  setting : constant String := Repo.repo_pubkey (R);
               begin
                  if not IsBlank (setting) then
                     print_extconfig ("pubkey", setting, True);
                  end if;
               end;
               case Repo.repo_ipv_type (R) is
                  when Repo.REPO_FLAGS_DEFAULT => null;
                  when Repo.REPO_FLAGS_LIMIT_IPV4 | Repo.REPO_FLAGS_LIMIT_IPV6 =>
                     print_extconfig ("ip_version", Repo.repo_ipv_type (R), False);
               end case;
               print_extconfig ("priority", Repo.repo_priority (R), False, True);
               TIO.Put_Line ("  }");
            end;
         end loop;
      end if;
   end show_repository_info;


   --------------------------------------------------------------------
   --  do_activation_test
   --------------------------------------------------------------------
   function do_activation_test return Boolean
   is
      result : Status.Activation_Status_Output;
   begin
      result := Status.ravensw_status;

      case result.status is
         when Status.ACT_STATUS_NODB =>
            TIO.Put_Line (TIO.Standard_Error, progname & " database doesn't exist.");
         when Status.ACT_STATUS_BAD_DB =>
            TIO.Put_Line (TIO.Standard_Error, progname & " database exists but doesn't work.");
         when Status.ACT_STATUS_NOPACKAGES =>
            TIO.Put_Line (TIO.Standard_Error, "no packages registered.");
         when Status.ACT_STATUS_ACTIVE =>
            TIO.Put_Line (int2str (result.count) & " packages installed.");
      end case;

      case result.status is
         when Status.ACT_STATUS_ACTIVE =>
            return True;
         when others =>
            return False;
      end case;
   end do_activation_test;

end Cmd.Unset;
