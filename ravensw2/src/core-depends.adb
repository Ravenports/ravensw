--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


package body Core.Depends is

   --------------------------------------------------------------------
   --  string_to_operator
   --------------------------------------------------------------------
   function string_to_operator (instr : String) return Dep_Version_Operator
   is
      result : Dep_Version_Operator := VERSION_ANY;
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
   end string_to_operator;

end Core.Depends;
