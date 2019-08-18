--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;     use Core.Pkg;
with Core.Strings; use Core.Strings;

package body Cmd.Unset is

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
                            flags    => comline.global_init_flags);
         return extended_version_info;
      end if;
      if comline.glob_list then
         return list_commands;
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
   function extended_version_info return Boolean
   is
   begin
      TIO.Put_Line (format_extconfig ("Version", progversion));
      TIO.Put_Line (pkg_config_dump);

      --  TODO: show_repository_info
      return True;
   end extended_version_info;


   --------------------------------------------------------------------
   --  format_extconfig
   --------------------------------------------------------------------
   function format_extconfig (name, value : String) return String
   is
      width     : constant Natural := 24;
      namespace : String (1 .. width) := (others => ' ');
      namelen   : Natural := name'Length;
   begin
      if namelen > width then
         namelen := width;
      end if;
      namespace (1 .. namelen) := name (name'First .. name'First + namelen - 1);
      return namespace & ": " & value;
   end format_extconfig;


   --------------------------------------------------------------------
   --  list_command
   --------------------------------------------------------------------
   function list_commands return Boolean
   is
   begin
      for command in Command_verb'Range loop
         if command /= Command_verb'First then
            TIO.Put_Line (convert_command_enum_to_label (command));
         end if;
      end loop;
      return True;
   end list_commands;

end Cmd.Unset;
