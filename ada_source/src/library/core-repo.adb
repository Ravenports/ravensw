--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Repo is

   --------------------------------------------------------------------
   --  mode_sets_write_flag
   --------------------------------------------------------------------
   function mode_sets_write_flag (mode : mode_t) return Boolean is
   begin
      case mode is
         when 2#0010# | 2#0011# | 2#0110# | 2#0111# => return True;  -- 2,3,6,7
         when 2#1010# | 2#1011# | 2#1110# | 2#1111# => return True;  -- 10,11,14,15
         when others => return False;
      end case;
   end mode_sets_write_flag;

end Core.Repo;
