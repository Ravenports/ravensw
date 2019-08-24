--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Deps is

   --------------------------------------------------------------------
   --  pkg_deps_string_toop
   --------------------------------------------------------------------
   function pkg_deps_string_toop (instr : String) return pkg_dep_version_op
   is
      result : pkg_dep_version_op := VERSION_ANY;
   begin
      case instr'Length is
         when 1 =>
            if instr = ">" then
               result := VERSION_GT;
            elsif instr = "<" then
               result := VERSION_LT;
            elsif instr = "!" then
               result := VERSION_NOT;
            elsif instr = "=" then
               result := VERSION_EQ;
            end if;
         when 2 =>
            if instr = ">=" then
               result := VERSION_GE;
            elsif instr = "<=" then
               result := VERSION_LE;
            elsif instr = "!=" then
               result := VERSION_NOT;
            elsif instr = "==" then
               result := VERSION_EQ;
            end if;
         when others =>
            null;
      end case;
      return result;
   end pkg_deps_string_toop;


end Core.Deps;
