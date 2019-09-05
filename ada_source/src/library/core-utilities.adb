--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings; use Core.Strings;
with Core.Unix;

package body Core.Utilities is

   --------------------------------------------------------------------
   --  pkg_absolutepath
   --------------------------------------------------------------------
   function pkg_absolutepath (input_path : String; fromroot : Boolean) return String
   is
      dest : Text;
      slash : constant String := "/";
   begin
      if input_path'Length = 0 then
         return slash;
      else
         if input_path (input_path'First) = '/' then
            if input_path'Length = 1 then
               return slash;
            end if;
         else
            --  We have a relative path here
            if not fromroot then
               dest := SUS (Unix.get_current_working_directory);
               if IsBlank (dest) then  --  problem
                  return "";
               end if;
            end if;
         end if;
      end if;

      declare
         index : Natural := input_path'First;
         ND    : constant Natural := input_path'Last;
         slash_present : Boolean;
         fragment  : Text;
         fraglen   : Natural;
         skip_rest : Boolean;
      begin
         loop
            skip_rest := False;
            slash_present := contains (input_path (index .. ND), slash);
            if slash_present then
               fragment := SUS (part_1 (input_path (index .. ND), slash));
            else
               fragment := SUS (input_path (index .. ND));
            end if;
            fraglen := SU.Length (fragment);

            --  check for special cases "", "." and ".."
            if fraglen = 0 then --  shouldn't happen (?)
               skip_rest := True;
            elsif fraglen = 1 and then input_path (index) = '.' then
               skip_rest := True;
            elsif fraglen = 2 and then input_path (index .. index + 1) = ".." then
               --  Remove entry from destination, we're drilling back up
               dest := SUS (head (USS (dest), slash));
               skip_rest := True;
            end if;
            index := index + fraglen + 1;

            if not skip_rest then
               SU.Append (dest, slash);
               SU.Append (dest, fragment);
            end if;
            exit when not slash_present;
         end loop;
      end;

      return USS (dest);

   end pkg_absolutepath;

end Core.Utilities;
