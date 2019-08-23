--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Core.Pkg;     use Core.Pkg;
with Core.Strings; use Core.Strings;
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

      --  Below this line, configuration is needed (invoke pkg_ini)
      if pkg_ini (path     => USS (comline.glob_config_file),
                  reposdir => USS (comline.glob_repo_config_dir),
                  flags    => comline.global_init_flags,
                  dlevel   => comline.glob_debug,
                  options  => USS (comline.glob_option)) /= EPKG_OK
      then
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
      TIO.Put_Line (pkg_config_dump);

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
      cols_plus1  : constant Natural := numwords mod cols'Range_Length;

      col_length  : array (cols) of Natural := (others => minlength);
      print_order : array (1 .. numwords) of Command_verb;

      column : cols := cols'First;
   begin
      if cols_plus1 > 0 then
         for N in 1 .. cols_plus1 loop
            col_length (cols (N)) := minlength + 1;
         end loop;
      end if;

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
   procedure show_repository_info is
      procedure list (position : pkg_repos_priority_crate.Cursor);
      procedure list (position : pkg_repos_priority_crate.Cursor)
      is
         key     : Text := pkg_repos_priority_crate.Element (position).reponame;
         rcursor : pkg_repos_crate.Cursor := repositories.Find (key);
         repo    : T_pkg_repo renames pkg_repos_crate.Element (rcursor);
      begin
         TIO.Put_Line ("  " & USS (repo.name) & ": {");
         print_extconfig ("url", pkg_repo_url (repo), True);
         print_extconfig ("enabled", pkg_repo_enabled (repo), False);
         if pkg_repo_mirror_type (repo) /= NOMIRROR then
            print_extconfig ("mirror_type", pkg_repo_mirror_type (repo), True);
         end if;
         if pkg_repo_signature_type (repo) /= SIG_NONE then
            print_extconfig ("signature_type", pkg_repo_signature_type (repo), True);
         end if;
         if not IsBlank (pkg_repo_fingerprints (repo)) then
            print_extconfig ("fingerprints", pkg_repo_fingerprints (repo), True);
         end if;
         if not IsBlank (pkg_repo_pubkey (repo)) then
            print_extconfig ("pubkey", pkg_repo_pubkey (repo), True);
         end if;
         if pkg_repo_ipv_type (repo) /= REPO_FLAGS_DEFAULT then
            print_extconfig ("ip_version", pkg_repo_ipv_type (repo), False);
         end if;
         print_extconfig ("priority", pkg_repo_priority_type (repo), False, True);
         TIO.Put_Line ("  }");
      end list;
   begin
      TIO.Put_Line ("Repositories:");
      Config.repositories_order.Iterate (list'Access);
   end show_repository_info;


   --------------------------------------------------------------------
   --  do_activation_test
   --------------------------------------------------------------------
   function do_activation_test return Boolean
   is
      result : Status.Pkg_Status_Output;
   begin
      result := Status.ravensw_status;

      case result.status is
         when Status.PKG_STATUS_NODB =>
            TIO.Put_Line (TIO.Standard_Error, progname & " database doesn't exist.");
         when Status.PKG_STATUS_BAD_DB =>
            TIO.Put_Line (TIO.Standard_Error, progname & " database exists but doesn't work.");
         when Status.PKG_STATUS_NOPACKAGES =>
            TIO.Put_Line (TIO.Standard_Error, "no packages registered.");
         when Status.PKG_STATUS_ACTIVE =>
            TIO.Put_Line (int2str (result.count) & " packages installed.");
      end case;

      case result.status is
         when Status.PKG_STATUS_ACTIVE => return True;
         when others            => return False;
      end case;

   end do_activation_test;

end Cmd.Unset;
