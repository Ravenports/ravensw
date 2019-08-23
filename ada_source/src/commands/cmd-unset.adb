--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Core.Pkg;     use Core.Pkg;
with Core.Strings; use Core.Strings;

package body Cmd.Unset is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  execute_no_command
   --------------------------------------------------------------------
   function execute_no_command (comline : Cldata) return Boolean
   is
      result : Pkg_Error_Type;
   begin
      if comline.glob_version = 1 then
         return basic_version_info;
      end if;
      if comline.glob_version = 2 then
         result := pkg_ini (path     => USS (comline.glob_config_file),
                            reposdir => USS (comline.glob_repo_config_dir),
                            flags    => comline.global_init_flags,
                            dlevel   => comline.glob_debug);
         if result /= EPKG_OK then
            return False;
         end if;
         return extended_version_info;
      end if;
      if comline.glob_list then
         return list_available_commands;
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
      column : cols := cols'First;
   begin
      for command in Command_verb'Range loop
         declare
            C : constant String := convert_command_enum_to_label (command);
         begin
            case command is
               when cv_unset => null;
               when others =>
                  TIO.Put (C);
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

end Cmd.Unset;
