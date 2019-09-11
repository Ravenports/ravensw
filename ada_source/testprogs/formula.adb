--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with Ada.Text_IO;
with Core.Event;
with Core.Deps;
with Core.Pkg;     use Core.Pkg;
with Core.Strings; use Core.Strings;
with Core.Unix;

use Core;

--  input: arg1 = operation (parse, sql, ops)
--         arg2 = "default" or <string to parse | sql > (not used for ops)
--         arg3 = parse: not used, sql: expected translation, ops: not used


procedure formula is

   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;

   type action_type is (parse, sql, ops);

   action : action_type;

   usage : String := "Usage: formula <parse|sql|ops> ['default'|string] [sql expected]";

   --  There's no input for ops check, just run through all of them
   procedure do_ops_check;

   --  If instr=default, run through the default tests, otherwise test input
   procedure check_parsing (instr : String);

   --  if instr=default, ignore expected and run through default tests
   procedure check_sql (instr : String; expected : String);

   function event_callback (eventx : Event.pkg_event; data : Text) return Boolean;
   procedure regevent;

   procedure do_ops_check
   is
      type case_type is range 1 .. 11;
      function get_input (test : case_type) return String;
      function get_input (test : case_type) return String is
      begin
         case test is
            when  1 => return "=";
            when  2 => return "==";
            when  3 => return ">=";
            when  4 => return ">";
            when  5 => return "<=";
            when  6 => return "<";
            when  7 => return "!";
            when  8 => return "!=";
            when  9 => return "*";
            when 10 => return "";
            when 11 => return "=>";
         end case;
      end get_input;
   begin
      for test in case_type'Range loop
         declare
            instr : String := get_input (test);
            operator : Deps.pkg_dep_version_op;
            exp_op   : Deps.pkg_dep_version_op;

            use type Deps.pkg_dep_version_op;
         begin
            operator := Deps.pkg_deps_string_toop (instr);
            case test is
               when  1 => exp_op := Deps.VERSION_EQ;
               when  2 => exp_op := Deps.VERSION_EQ;
               when  3 => exp_op := Deps.VERSION_GE;
               when  4 => exp_op := Deps.VERSION_GT;
               when  5 => exp_op := Deps.VERSION_LE;
               when  6 => exp_op := Deps.VERSION_LT;
               when  7 => exp_op := Deps.VERSION_NOT;
               when  8 => exp_op := Deps.VERSION_NOT;
               when  9 => exp_op := Deps.VERSION_ANY;
               when 10 => exp_op := Deps.VERSION_ANY;
               when 11 => exp_op := Deps.VERSION_ANY;
            end case;
            if operator /= exp_op then
               TIO.Put_Line ("operator test" & test'Img & "failed:");
               TIO.Put_Line ("input: " & instr);
               TIO.Put_Line ("expected: " & exp_op'Img);
               TIO.Put_Line ("received: " & operator'Img);
               return;
            end if;
         end;
      end loop;
      TIO.Put_Line ("All operator tests passed");
   end do_ops_check;


   procedure check_parsing (instr : String)
   is
      type case_type is range 1 .. 7;
      function get_input (test : case_type) return String;
      function get_input (test : case_type) return String is
      begin
         case test is
            when 1 => return "name";
            when 2 => return "name = 1.0";
            when 3 => return "name >= 1.0,1";
            when 4 => return "name1, name2";
            when 5 => return "name1 | name2, name3";
            when 6 => return "name1 = 1.0 | name2 != 1.0, name3 > 1.0 < 2.0 != 1.5";
            when 7 =>
               return "name1 = 1.0 | name2 != 1.0, name3 > 1.0 < 2.0 != 1.5, name4 +opt1 -opt2";
         end case;
      end get_input;
   begin
      if instr = "default" then
         for test in case_type'Range loop
            declare
               teststr : String := get_input (test);
               formula : Deps.formula_crate.Vector;
            begin
               formula := Deps.pkg_deps_parse_formula (teststr);
               declare
                  backout : String := Deps.pkg_deps_formula_tostring (formula);
               begin
                  if teststr = backout then
                     TIO.Put_Line ("PASSED: " & teststr);
                  else
                     TIO.Put_Line ("FAILED: " & teststr);
                     TIO.Put_Line ("     => " & backout);
                  end if;
               end;
            end;
         end loop;
      else
         declare
            formula : Deps.formula_crate.Vector;
         begin
            formula := Deps.pkg_deps_parse_formula (instr);
            declare
               backout : String := Deps.pkg_deps_formula_tostring (formula);
            begin
               if instr = backout then
                  TIO.Put_Line ("PASSED: " & instr);
               else
                  TIO.Put_Line ("FAILED: " & instr);
                  TIO.Put_Line ("     => " & backout);
               end if;
            end;
         end;
      end if;
   end check_parsing;

   procedure check_sql (instr : String; expected : String)
   is
      type case_type is range 1 .. 5;
      function get_input  (test : case_type) return String;
      function exp_output (test : case_type) return String;

      function get_input (test : case_type) return String is
      begin
         case test is
            when 1 => return "name";
            when 2 => return "name = 1.0";
            when 3 => return "name >= 1.0,1";
            when 4 => return "name1 | name2";
            when 5 => return "name1 = 1.0 | name2 != 1.0";
         end case;
      end get_input;

      function exp_output (test : case_type) return String is
      begin
         case test is
            when 1 => return "(name='name')";
            when 2 => return "(name='name' AND vercmp('=',version,'1.0'))";
            when 3 => return "(name='name' AND vercmp('>=',version,'1.0,1'))";
            when 4 => return "(name='name1') OR (name='name2')";
            when 5 => return "(name='name1' AND vercmp('=',version,'1.0')) OR " &
                 "(name='name2' AND vercmp('!=',version,'1.0'))";
         end case;
      end exp_output;
   begin
      if instr = "default" then
         for test in case_type'Range loop
            declare
               formula : Deps.formula_crate.Vector;
            begin
               formula := Deps.pkg_deps_parse_formula (get_input (test));
               declare
                  backout : String := Deps.pkg_deps_formula_tosql (formula.First_Element.items);
               begin
                  if backout = exp_output (test) then
                     TIO.Put_Line ("PASSED: " & get_input (test));
                  else
                     TIO.Put_Line ("FAILED: " & get_input (test));
                     TIO.Put_Line ("     => " & exp_output (test));
                  end if;
               end;
            end;
         end loop;
      else
         declare
            formula : Deps.formula_crate.Vector;
         begin
            formula := Deps.pkg_deps_parse_formula (instr);
            declare
               backout : String := Deps.pkg_deps_formula_tosql (formula.First_Element.items);
            begin
               if backout = expected then
                  TIO.Put_Line ("PASSED: " & instr);
               else
                  TIO.Put_Line ("FAILED: " & instr);
               end if;
               TIO.Put_Line ("     => " & backout);
            end;
         end;
      end if;
   end check_sql;

   function event_callback (eventx : Event.pkg_event; data : Text) return Boolean
   is
   begin
      case eventx.this_event is

         when Event.PKG_EVENT_ERRNO =>
            TIO.Put_Line
              (TIO.Standard_Error,
               (USS (eventx.err_function) & '(' & USS (eventx.err_argument) & "): " &
                  Unix.strerror (eventx.err_number)));

         when Event.PKG_EVENT_ERROR =>
            TIO.Put_Line (TIO.Standard_Error, (USS (eventx.message)));

         when Event.PKG_EVENT_NOTICE =>
            TIO.Put_Line (USS (eventx.message));

         when Event.PKG_EVENT_DEVELOPER_MODE =>
            TIO.Put_Line (TIO.Standard_Error, ("DEVELOPER_MODE: " & USS (eventx.message)));

         when others => null;
      end case;
      return True;
   end event_callback;

   procedure regevent is
   begin
      Event.pkg_event_register (callback      => event_callback'Unrestricted_Access,
                                callback_data => blank);
   end regevent;
begin

   regevent;

   if CLI.Argument_Count < 1 then
      TIO.Put_Line ("Not enough arguments. " & usage);
      return;
   end if;

   declare
      action_arg : String renames CLI.Argument (1);
   begin
      if action_arg = "parse" then
         action := parse;
      elsif action_arg = "sql" then
         action := sql;
      elsif action_arg = "ops" then
         action := ops;
      else
         TIO.Put_Line ("Wrong first argument. " & usage);
         return;
      end if;

      if action = ops then
         do_ops_check;
         return;
      end if;

      if action = parse then
         if CLI.Argument_Count < 2 then
            TIO.Put_Line ("Not enough arguments. " & usage);
            return;
         end if;
         check_parsing (CLI.Argument (2));
         return;
      end if;

      if action = sql then
         if CLI.Argument_Count < 2 then
            TIO.Put_Line ("Not enough arguments. " & usage);
            return;
         else
            if CLI.Argument (2) = "default" then
               check_sql ("default", "");
            else
               if CLI.Argument_Count < 3 then
                  TIO.Put_Line ("Not enough arguments. " & usage);
                  return;
               else
                  check_sql (CLI.Argument (2), CLI.Argument (3));
                  return;
               end if;
            end if;
         end if;
      end if;

   end;
end formula;
