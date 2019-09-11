--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Core.Strings;
with Core.Event;

use Core.Strings;

package body Core.Deps is

   package LAT renames Ada.Characters.Latin_1;

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


   --------------------------------------------------------------------
   --  pkg_deps_parse_formula
   --------------------------------------------------------------------
   function pkg_deps_parse_formula (instr : String) return formula_crate.Vector
   is
      procedure delete_formula_item is new Ada.Unchecked_Deallocation
        (pkg_dep_formula_item, pkg_dep_formula_item_Access);

      procedure delete_formula is new Ada.Unchecked_Deallocation
        (pkg_formula, pkg_formula_Access);

      procedure alpha;
      procedure bravo;
      procedure charlie;
      procedure echo;
      procedure foxtrot;
      procedure golf;
      procedure hotel;
      procedure india;
      procedure juliet;
      procedure kilo;
      procedure lima;

      formula  : formula_crate.Vector;
      cur_op   : pkg_dep_version_op := VERSION_ANY;
      test_op  : pkg_dep_version_op;
      cur      : pkg_formula_Access;
      cur_item : pkg_dep_formula_item_Access;
      opt_on   : Boolean;

      type state_state is
        (st_parse_dep_name,
         st_parse_after_name,
         st_parse_ver_op,
         st_parse_after_op,
         st_parse_version_number,
         st_parse_after_version,
         st_parse_option_start,
         st_parse_option,
         st_parse_after_option,
         st_parse_comma,
         st_parse_or,
         st_skip_spaces,
         st_error);

      state      : state_state := st_parse_dep_name;
      next_state : state_state := st_parse_dep_name;
      instrnull  : constant String (1 .. instr'Length + 1) := instr & LAT.NUL;

      P : Integer;   --  Index
      C : Integer;   --  Index

      --   ==============
      --   ===  ALPHA  ==
      --   ==============
      procedure alpha is
      begin
         case instrnull (P) is
            when LAT.Space |
                 LAT.NUL =>
               state := st_skip_spaces;

               if P = C then
                  --  Spaces at the beginning
                  next_state := st_parse_dep_name;
               else
                  --  Spaces after the name
                  cur_item := new pkg_dep_formula_item;
                  cur_item.name := SUS (instrnull (C .. P - 1));
                  next_state := st_parse_after_name;
               end if;

            when LAT.Comma =>
               if P = C then
                  state := st_error;
               else
                  cur_item := new pkg_dep_formula_item;
                  cur_item.name := SUS (instrnull (C .. P - 1));
                  state := st_parse_after_name;
               end if;

            when LAT.Exclamation .. LAT.Plus_Sign |
               LAT.Minus_Sign .. LAT.Tilde =>
               --  equivalent to isprint() but excluding the comma
               P := P + 1;

            when others =>
               state := st_error;

         end case;
      end alpha;


      --   ==============
      --   ===  BRAVO  ==
      --   ==============
      procedure bravo is
      begin
         case instrnull (P) is
            when LAT.Comma |
                 LAT.NUL =>
               state := st_parse_comma;

            when LAT.Vertical_Line =>
               state := st_parse_or;

            when LAT.Plus_Sign |
                 LAT.Minus_Sign =>
               C := P;
               state := st_parse_option_start;

            when LAT.Greater_Than_Sign |
                 LAT.Less_Than_Sign |
                 LAT.Equals_Sign |
                 LAT.Exclamation =>
               C := P;
               cur_op := VERSION_ANY;
               state := st_parse_ver_op;

            when others =>
               state := st_error;

         end case;
      end bravo;

      --   ================
      --   ===  CHARLIE  ==
      --   ================
      procedure charlie is
      begin
         case instrnull (P) is

            when LAT.Greater_Than_Sign |
                 LAT.Less_Than_Sign |
                 LAT.Equals_Sign |
                 LAT.Exclamation =>
               P := P + 1;

            when others =>
               if P - C = 2 then
                  if C + 1 > instrnull'Last then
                     state := st_error;
                  else
                     test_op := pkg_deps_string_toop (instrnull (C .. C + 1));
                     if test_op = VERSION_ANY then
                        state := st_error;
                     else
                        cur_op := test_op;
                     end if;
                  end if;
               elsif P - C = 1 then
                  test_op := pkg_deps_string_toop (instrnull (C .. C));
                  if test_op = VERSION_ANY then
                     state := st_error;
                  else
                     cur_op := test_op;
                  end if;
               else
                  state := st_error;
               end if;

               if state /= st_error then
                  state := st_skip_spaces;
                  next_state := st_parse_after_op;
               end if;

         end case;
      end charlie;


      --   =============
      --   ===  ECHO  ==
      --   =============
      procedure echo is
      begin
         if cur_op = VERSION_ANY then
            state := st_error;
         else
            state := st_parse_version_number;
         end if;
      end echo;


      --   ================
      --   ===  FOXTROT  ==
      --   ================
      procedure foxtrot
      is
         use_main : Boolean;
      begin
         case instrnull (P) is
            when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' |
                 LAT.Minus_Sign |
                 LAT.Low_Line |
                 LAT.Full_Stop =>
               P := P + 1;
               use_main := False;

            when LAT.Comma =>
               case instrnull (P + 1) is
                  when '0' .. '9' =>
                     P := P + 1;
                     use_main := False;
                  when others =>
                     use_main := True;
               end case;

            when others =>
               use_main := True;

         end case;

         if use_main then
            if P - C > 0 then
               declare
                  VI : pkg_dep_version_item;
               begin
                  VI.version := SUS (instrnull (C .. P - 1));
                  VI.op := cur_op;
                  cur_item.versions.Append (VI);
               end;
               state := st_skip_spaces;
               next_state := st_parse_after_version;
            else
               state := st_error;
            end if;
         end if;
      end foxtrot;


      --   =============
      --   ===  GOLF  ==
      --   =============
      procedure golf is
      begin
         case instrnull (P) is
            when LAT.Plus_Sign =>
               opt_on := True;
            when others =>
               opt_on := False;
         end case;
         P := P + 1;
         C := P;
         state := st_parse_option;
      end golf;


      --   ==============
      --   ===  HOTEL  ==
      --   ==============
      procedure hotel is
      begin
         case instrnull (P) is
            when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' |
                 LAT.Minus_Sign |
                 LAT.Low_Line =>
               P := P + 1;
            when others =>
               if P - C > 0 then
                  declare
                     OI : pkg_dep_option_item;
                  begin
                     OI.option := SUS (instrnull (C .. P - 1));
                     OI.active := opt_on;
                     cur_item.options.Append (OI);
                  end;
                  state := st_skip_spaces;
                  next_state := st_parse_after_option;
               else
                  state := st_error;
               end if;
         end case;
      end hotel;


      --   ==============
      --   ===  INDIA  ==
      --   ==============
      procedure india is
      begin
         if cur = null then
            cur := new pkg_formula;
         end if;

         cur.items.Append (cur_item.all);
         formula.Append (cur.all);

         delete_formula_item (cur_item);
         delete_formula (cur);

         P := P + 1;
         state := st_skip_spaces;
         next_state := st_parse_dep_name;
      end india;


      --   ===============
      --   ===  JULIET  ==
      --   ===============
      procedure juliet is
      begin
         if cur = null then
            cur := new pkg_formula;
         end if;

         cur.items.Append (cur_item.all);
         delete_formula_item (cur_item);

         P := P + 1;
         state := st_skip_spaces;
         next_state := st_parse_dep_name;
      end juliet;


      --   =============
      --   ===  KILO  ==
      --   =============
      procedure kilo is
      begin
         case instrnull (P) is
            when LAT.Space =>
               P := P + 1;
            when LAT.NUL =>
               state := st_parse_comma;
            when others =>
               C := P;
               state := next_state;
         end case;
      end kilo;


      --   =============
      --   ===  LIMA  ==
      --   =============
      procedure lima is
      begin
         Event.pkg_emit_error (SUS ("Lima: Cannot parse pkg formula: " & instrnull));
         formula.Clear;
         if cur_item /= null then
            delete_formula_item (cur_item);
         end if;
         if cur /= null then
            delete_formula (cur);
         end if;
      end lima;

   begin
      P := instrnull'First;
      C := instrnull'First;

      loop
         exit when P > instrnull'Last;

         case state is
            when st_parse_dep_name        => alpha;
            when st_parse_after_name |
                 st_parse_after_version |
                 st_parse_after_option    => bravo;
            when st_parse_ver_op          => charlie;
            when st_parse_after_op        => echo;
            when st_parse_version_number  => foxtrot;
            when st_parse_option_start    => golf;
            when st_parse_option          => hotel;
            when st_parse_comma           => india;
            when st_parse_or              => juliet;
            when st_skip_spaces           => kilo;
            when st_error                 => lima;  return formula;
         end case;
      end loop;

      if state /= st_skip_spaces and then
        state /= st_parse_comma
      then
         Event.pkg_emit_error (SUS ("Later: Cannot parse pkg formula: " & instr));
         formula.Clear;
      end if;

      if cur_item /= null then
         delete_formula_item (cur_item);
      end if;
      if cur /= null then
         delete_formula (cur);
      end if;

      return formula;
   end pkg_deps_parse_formula;


   --------------------------------------------------------------------
   --  pkg_deps_formula_tosql
   --------------------------------------------------------------------
   function pkg_deps_formula_tosql (FI   : pkg_dep_formula_item;
                                    last : Boolean) return String
   is
      procedure scan_version (position : pkg_dep_version_item_crate.Cursor);

      result : Text := SUS ("(name='" & USS (FI.name) & "'");

      procedure scan_version (position : pkg_dep_version_item_crate.Cursor)
      is
         rec : pkg_dep_version_item renames pkg_dep_version_item_crate.Element (position);
      begin
         SU.Append (result, " AND vercmp('" & pkg_deps_op_tostring (rec.op) & "',version,'" &
                      USS (rec.version) & "')");
      end scan_version;
   begin
      FI.versions.Iterate (scan_version'Access);
      if last then
         SU.Append (result, ")");
      else
         SU.Append (result, ") OR ");
      end if;
      return USS (result);
   end pkg_deps_formula_tosql;


   --------------------------------------------------------------------
   --  pkg_deps_formula_tosql
   --------------------------------------------------------------------
   function pkg_deps_formula_tosql (Fitems : pkg_dep_formula.Vector) return String
   is
      procedure scan_item (item_position : pkg_dep_formula.Cursor);

      last_item    : pkg_dep_formula.Cursor;
      result       : Text;

      procedure scan_item (item_position : pkg_dep_formula.Cursor)
      is
         item : pkg_dep_formula_item renames pkg_dep_formula.Element (item_position);
         use type pkg_dep_formula.Cursor;
      begin
         SU.Append (result, pkg_deps_formula_tosql (item, (item_position = last_item)));
      end scan_item;

   begin
      last_item := Fitems.Last;
      Fitems.Iterate (scan_item'Access);

      return USS (result);
   end pkg_deps_formula_tosql;


   --------------------------------------------------------------------
   --  pkg_deps_formula_tosql
   --------------------------------------------------------------------
   function pkg_deps_op_tostring (op : pkg_dep_version_op) return String is
   begin
      case op is
         when VERSION_ANY => return "?";
         when VERSION_EQ  => return "=";
         when VERSION_LT  => return "<";
         when VERSION_GT  => return ">";
         when VERSION_LE  => return "<=";
         when VERSION_GE  => return ">=";
         when VERSION_NOT => return "!=";
      end case;
   end pkg_deps_op_tostring;


   --------------------------------------------------------------------
   --  pkg_deps_formula_tostring
   --------------------------------------------------------------------
   function pkg_deps_formula_tostring (F : formula_crate.Vector) return String
   is
      procedure scan_formula (position : formula_crate.Cursor);
      procedure scan_item (item_position : pkg_dep_formula.Cursor);
      procedure scan_item_version (iv_position : pkg_dep_version_item_crate.Cursor);
      procedure scan_item_option  (io_position : pkg_dep_option_item_crate.Cursor);

      result : Text;
      last_formula : formula_crate.Cursor;
      last_item    : pkg_dep_formula.Cursor;

      procedure scan_formula (position : formula_crate.Cursor)
      is
         formula : pkg_formula renames formula_crate.Element (position);
         use type formula_crate.Cursor;
      begin
         last_item := formula.items.Last;
         formula.items.Iterate (scan_item'Access);
         if position /= last_formula then
            SU.Append (result, ", ");
         end if;
      end scan_formula;

      procedure scan_item (item_position : pkg_dep_formula.Cursor)
      is
         item : pkg_dep_formula_item renames pkg_dep_formula.Element (item_position);
         use type pkg_dep_formula.Cursor;
      begin
         SU.Append (result, item.name);
         item.versions.Iterate (scan_item_version'Access);
         item.options.Iterate (scan_item_option'Access);
         if item_position /= last_item then
            SU.Append (result, " | ");
         end if;
      end scan_item;

      procedure scan_item_version (iv_position : pkg_dep_version_item_crate.Cursor)
      is
         iv : pkg_dep_version_item renames pkg_dep_version_item_crate.Element (iv_position);
      begin
         SU.Append (result, " " & pkg_deps_op_tostring (iv.op) & " " & USS (iv.version));
      end scan_item_version;

      procedure scan_item_option  (io_position : pkg_dep_option_item_crate.Cursor)
      is
         io : pkg_dep_option_item renames pkg_dep_option_item_crate.Element (io_position);
      begin
         if io.active then
            SU.Append (result, " +" & USS (io.option));
         else
            SU.Append (result, " -" & USS (io.option));
         end if;
      end scan_item_option;

   begin
      last_formula := F.Last;
      F.Iterate (scan_formula'Access);
      return USS (result);
   end pkg_deps_formula_tostring;

end Core.Deps;
