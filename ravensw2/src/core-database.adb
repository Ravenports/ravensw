--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


package body Core.Database is

   --------------------------------------------------------------------
   --  set_case_sensitivity
   --------------------------------------------------------------------
   procedure set_case_sensitivity (sensitive : Boolean) is
   begin
      case_sensitivity_setting := sensitive;
   end set_case_sensitivity;


   --------------------------------------------------------------------
   --  case_sensitivity_is_on
   --------------------------------------------------------------------
   function case_sensitivity_is_on return Boolean is
   begin
      return case_sensitivity_setting;
   end case_sensitivity_is_on;


   --------------------------------------------------------------------
   --  set_match_behavior
   --------------------------------------------------------------------
   function set_match_behavior
     (request_exact : Boolean := False;
      request_glob  : Boolean := False;
      request_regex : Boolean := False;
      request_condition : Boolean := False) return Match_Behavior is
   begin
      if request_exact then
         return MATCH_EXACT;
      elsif request_glob then
         return MATCH_GLOB;
      elsif request_regex then
         return MATCH_REGEX;
      elsif request_condition then
         return MATCH_CONDITION;
      else
         return MATCH_ALL;
      end if;
   end set_match_behavior;


   --------------------------------------------------------------------
   --  verbatim_command
   --------------------------------------------------------------------
   procedure verbatim_command (passthrough : String) is
   begin
      null;  --  TODO: Implement
   end verbatim_command;



end Core.Database;
