--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Directories;
with Core.Checksum;  use Core.Checksum;
with Core.Pkg;       use Core.Pkg;

--  input: arg1 = operation (file, validate, generate, symlink)
--         arg2 = filename
--         arg3 = encoding (0 .. 6) for
--    (PKG_HASH_TYPE_SHA256_BASE32,
--         PKG_HASH_TYPE_SHA256_HEX,
--         PKG_HASH_TYPE_BLAKE2_BASE32,
--         PKG_HASH_TYPE_SHA256_RAW,
--         PKG_HASH_TYPE_BLAKE2_RAW,
--         PKG_HASH_TYPE_BLAKE2S_BASE32,
--         PKG_HASH_TYPE_BLAKE2S_RAW,

procedure checksum is

   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;

   action_arg : String renames CLI.Argument (1);
   filename   : String renames CLI.Argument (2);
   arg3       : String renames CLI.Argument (3);

   type action_type is (file, validate, generate, symlink);

   action : action_type;

   usage : String :=
     "Usage: checksum <file|validate|generate|symlink> <filename> <encoding: 0 .. 6 | checksum>";

   hashtype : T_checksum_type := PKG_HASH_TYPE_SHA256_BASE32;

begin

   if CLI.Argument_Count < 3 then
      TIO.Put_Line ("Not enough arguments. " & usage);
      return;
   end if;

   if action_arg = "file" then
      action := file;
   elsif action_arg = "validate" then
      action := validate;
   elsif action_arg = "generate" then
      action := generate;
   elsif action_arg = "symlink" then
      action := symlink;
   else
      TIO.Put_Line ("Wrong first argument. " & usage);
      return;
   end if;

   if not DIR.Exists (filename) then
      TIO.Put_Line ("filename does not exist.");
      return;
   end if;

   case action is
      when symlink | generate | file =>
         declare
            number : Integer;
         begin
            number := Integer'Value (arg3);
            if number >= T_checksum_type'Pos (T_checksum_type'Last) and then
              number <=  T_checksum_type'Pos (T_checksum_type'Last)
            then
               hashtype := T_checksum_type'Val (number);
            end if;
         exception
            when others => null;
         end;
      when validate =>
         null;  --  this is a checksum
   end case;

   case action is
      when file     => TIO.Put_Line (pkg_checksum_file (filename, hashtype));
      when symlink  => TIO.Put_Line (pkg_checksum_symlink (filename, hashtype));
      when generate => TIO.Put_Line (pkg_checksum_generate_file (filename, hashtype));
      when validate =>
         if pkg_checksum_validate_file (filename, arg3) then
            TIO.Put_Line ("checksum matches");
         else
            TIO.Put_Line ("invalid checksum");
         end if;
   end case;

end checksum;
