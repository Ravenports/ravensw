--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Fixed;

package body Core.Strings is

   package AS renames Ada.Strings;


   --------------------------------------------------------------------------------------------
   --  USS
   --------------------------------------------------------------------------------------------
   function USS (US : Text) return String is
   begin
      return SU.To_String (US);
   end USS;


   --------------------------------------------------------------------------------------------
   --  SUS
   --------------------------------------------------------------------------------------------
   function SUS (S : String) return Text is
   begin
      return SU.To_Unbounded_String (S);
   end SUS;


   --------------------------------------------------------------------------------------------
   --  contains #1
   --------------------------------------------------------------------------------------------
   function contains (S : String; fragment : String) return Boolean is
   begin
      return (AS.Fixed.Index (Source => S, Pattern => fragment) > 0);
   end contains;


   --------------------------------------------------------------------------------------------
   --  contains #2
   --------------------------------------------------------------------------------------------
   function contains (US : Text; fragment : String) return Boolean is
   begin
      return (SU.Index (Source => US, Pattern => fragment) > 0);
   end contains;


   --------------------------------------------------------------------------------------------
   --  part_1
   --------------------------------------------------------------------------------------------
   function part_1 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (S'First .. slash - 1);
   end part_1;


   --------------------------------------------------------------------------------------------
   --  part_2
   --------------------------------------------------------------------------------------------
   function part_2 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (slash + separator'Length .. S'Last);
   end part_2;


   --------------------------------------------------------------------------------------------
   --  IsBlank #1
   --------------------------------------------------------------------------------------------
   function IsBlank (US : Text)   return Boolean is
   begin
      return SU.Length (US) = 0;
   end IsBlank;


   --------------------------------------------------------------------------------------------
   --  IsBlank #2
   --------------------------------------------------------------------------------------------
   function IsBlank (S  : String) return Boolean is
   begin
      return S'Length = 0;
   end IsBlank;


   --------------------------------------------------------------------------------------------
   --  replace_all
   --------------------------------------------------------------------------------------------
   function replace_all (S : String; reject, shiny : Character) return String
   is
      rejectstr : constant String (1 .. 1) := (1 => reject);
      returnstr : String := S;
      focus     : Natural;
   begin
      loop
         focus := AS.Fixed.Index (Source => returnstr, Pattern => rejectstr);
         exit when focus = 0;
         returnstr (focus) := shiny;
      end loop;
      return returnstr;
   end replace_all;


   --------------------------------------------------------------------------------------------
   --  replace_substring
   --------------------------------------------------------------------------------------------
   function replace_substring (US : Text;
                               old_string : String;
                               new_string : String) return Text
   is
      back_marker  : Natural := SU.Index (Source => US, Pattern => old_string);
      front_marker : Natural := back_marker + old_string'Length - 1;
   begin
      if back_marker = 0 then
         return US;
      end if;
      return SU.Replace_Slice (Source => US,
                               Low    => back_marker,
                               High   => front_marker,
                               By     => new_string);
   end replace_substring;


   --------------------------------------------------------------------------------------------
   --  int2str
   --------------------------------------------------------------------------------------------
   function int2str (A : Integer) return String
   is
      raw : constant String := A'Img;
      len : constant Natural := raw'Length;
   begin
      if A < 0 then
         return raw;
      else
         return raw (2 .. len);
      end if;
   end int2str;


   --------------------------------------------------------------------------------------------
   --  int2text
   --------------------------------------------------------------------------------------------
   function int2text (A : Integer) return Text is
   begin
      return SUS (int2str (A));
   end int2text;


   --------------------------------------------------------------------------------------------
   --  count_char
   --------------------------------------------------------------------------------------------
   function count_char (S : String; focus : Character) return Natural
   is
      result : Natural := 0;
   begin
      for x in S'Range loop
         if S (x) = focus then
            result := result + 1;
         end if;
      end loop;
      return result;
   end count_char;


   --------------------------------------------------------------------------------------------
   --  json_escape
   --------------------------------------------------------------------------------------------
   function json_escape (S : String) return String
   is
      num_quotes : Natural := count_char (S, ASCII.Quotation);
      num_slash  : Natural := count_char (S, '\');
   begin
      if num_quotes + num_slash = 0 then
         return S;
      end if;
      declare
         result : String (1 .. S'Length + num_quotes + num_slash);
         index  : Natural := result'First;
      begin
         for x in S'Range loop
            if S (x) = ASCII.Quotation or else S (x) = '\' then
               result (index) := '\';
               index := index + 1;
            end if;
            result (index) := S (x);
            index := index + 1;
         end loop;
         return result;
      end;
   end json_escape;


   --------------------------------------------------------------------------------------------
   --  leads #1
   --------------------------------------------------------------------------------------------
   function leads (S : String; fragment : String) return Boolean is
   begin
      if fragment'Length > S'Length then
         return False;
      end if;
      return (S (S'First .. S'First + fragment'Length - 1) = fragment);
   end leads;


   --------------------------------------------------------------------------------------------
   --  leads #2
   --------------------------------------------------------------------------------------------
   function leads (US : Text; fragment : String) return Boolean is
   begin
      return leads (USS (US), fragment);
   end leads;


   --------------------------------------------------------------------------------------------
   --  trails #1
   --------------------------------------------------------------------------------------------
   function trails (S : String; fragment : String) return Boolean is
   begin
      if fragment'Length > S'Length then
         return False;
      end if;
      return (S (S'Last - fragment'Length + 1 .. S'Last) = fragment);
   end trails;


   --------------------------------------------------------------------------------------------
   --  trails #2
   --------------------------------------------------------------------------------------------
   function trails (US : Text; fragment : String) return Boolean is
   begin
      return trails (USS (US), fragment);
   end trails;

end Core.Strings;
