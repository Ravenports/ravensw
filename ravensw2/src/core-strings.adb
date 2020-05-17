--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;

package body Core.Strings is

   package AS  renames Ada.Strings;
   package HAN renames Ada.Characters.Handling;
   package LAT renames Ada.Characters.Latin_1;
   package IIO renames Ada.Integer_Text_IO;

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


   --------------------------------------------------------------------------------------------
   --  specific_field
   --------------------------------------------------------------------------------------------
   function specific_field
     (S            : String;
      field_number : Positive;
      delimiter    : String := " ") return String
   is
      back  : Integer;
      dsize : Natural := delimiter'Length;
      front : Integer := S'First;
   begin
      for field in 1 .. field_number - 1 loop
         back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
         if back <= 0 then
            return "";
         end if;
         front := back + dsize;
      end loop;
      back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
      if back > 0 then
         return S (front .. back - 1);
      else
         return S (front .. S'Last);
      end if;
   end specific_field;


   --------------------------------------------------------------------------------------------
   --  uppercase #1
   --------------------------------------------------------------------------------------------
   function uppercase (US : Text) return Text
   is
      tall : String := uppercase (USS (US));
   begin
      return SUS (tall);
   end uppercase;


   --------------------------------------------------------------------------------------------
   --  uppercase #2
   --------------------------------------------------------------------------------------------
   function uppercase (S : String) return String is
   begin
      return HAN.To_Upper (S);
   end uppercase;


   --------------------------------------------------------------------------------------------
   --  lowercase #1
   --------------------------------------------------------------------------------------------
   function lowercase (US : Text) return Text
   is
      short : String := lowercase (USS (US));
   begin
      return SUS (short);
   end lowercase;


   --------------------------------------------------------------------------------------------
   --  lowercase #2
   --------------------------------------------------------------------------------------------
   function lowercase (S : String) return String is
   begin
      return HAN.To_Lower (S);
   end lowercase;


   --------------------------------------------------------------------------------------------
   --  start_index
   --------------------------------------------------------------------------------------------
   function start_index (S : String; fragment : String) return Natural
   is
   begin
      return AS.Fixed.Index (Source => S, Pattern => fragment);
   end start_index;


   --------------------------------------------------------------------------------------------
   --  octal
   --------------------------------------------------------------------------------------------
   function octal (number : Natural; places : Positive; zero_pad : Boolean) return String
   is
      result  : String (1 .. places) := (others => ' ');
      workstr : String (1 .. places + 3);
      max     : Natural := 8 ** places - 1;
      start   : Positive;
      index   : Natural;
   begin
      if number > max then
         result := (others => '#');
         return result;
      end if;

      if zero_pad then
         result := (others => '0');
      end if;

      --  function prefixes with "8#" and suffixes with "#"
      IIO.Put (To   => workstr,
               Item => number,
               Base => 8);
      index := start_index (workstr, "#");

      --  "8#777#"  => "777"
      --  " 8#77#"  => "077"
      --  "  8#7#   => "007"
      --   123456       123

      start := index - 1;
      result (start .. result'Last) := workstr (index + 1 .. workstr'Last - 1);
      return result;
   end octal;


   --------------------------------------------------------------------------------------------
   --  map_hash
   --------------------------------------------------------------------------------------------
   function map_hash (key : Text) return CON.Hash_Type is
   begin
      return AS.Hash (USS (key));
   end map_hash;


   --------------------------------------------------------------------------------------------
   --  equivalent #1
   --------------------------------------------------------------------------------------------
   function equivalent (A, B : Text) return Boolean
   is
      use type Text;
   begin
      return A = B;
   end equivalent;


   --------------------------------------------------------------------------------------------
   --  equivalent #2
   --------------------------------------------------------------------------------------------
   function equivalent (A : Text; B : String) return Boolean
   is
      A2S : constant String := USS (A);
   begin
      return A2S = B;
   end equivalent;


   --------------------------------------------------------------------------------------------
   --  pad_right
   --------------------------------------------------------------------------------------------
   function pad_right (S : String; places : Positive) return String
   is
      result : String (1 .. places) := (others => ' ');
   begin
      if S'Length > places then
         result := S (S'First .. S'First + places - 1);
      else
         result (1 .. S'Length) := S;
      end if;
      return result;
   end pad_right;


   --------------------------------------------------------------------------------------------
   --  pad_left
   --------------------------------------------------------------------------------------------
   function pad_left (S : String; places : Positive) return String
   is
      result : String (1 .. places) := (others => ' ');
   begin
      if S'Length > places then
         result := S (S'Last - places + 1 .. S'Last);
      else
         result (result'Last - S'Length + 1 .. result'Last) := S;
      end if;
      return result;
   end pad_left;


   --------------------------------------------------------------------------------------------
   --  head #1
   --------------------------------------------------------------------------------------------
   function head (US : Text; delimiter : Text) return Text
   is
      result : constant String := head (USS (US), USS (delimiter));
   begin
      return SUS (result);
   end head;


   --------------------------------------------------------------------------------------------
   --  head #2
   --------------------------------------------------------------------------------------------
   function head (S  : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return "";
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (back_marker .. front_marker - 1);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end head;


   --------------------------------------------------------------------------------------------
   --  tail #1
   --------------------------------------------------------------------------------------------
   function tail (US : Text; delimiter : Text) return Text
   is
      result : constant String := tail (USS (US), USS (delimiter));
   begin
      return SUS (result);
   end tail;


   --------------------------------------------------------------------------------------------
   --  tail #2
   --------------------------------------------------------------------------------------------
   function tail (S : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return S;
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (front_marker + dl_size .. S'Last);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end tail;



   --------------------------------------------------------------------
   --  DQ
   --------------------------------------------------------------------
   function DQ (txt : String) return String is
   begin
      return LAT.Quotation & txt & LAT.Quotation;
   end DQ;


   --------------------------------------------------------------------
   --  DQ
   --------------------------------------------------------------------
   function SQ (txt : String) return String is
   begin
      return LAT.Apostrophe & txt & LAT.Apostrophe;
   end SQ;


   --------------------------------------------------------------------------------------------
   --  trim
   --------------------------------------------------------------------------------------------
   function trim (S : String) return String is
   begin
      return AS.Fixed.Trim (S, AS.Both);
   end trim;


   --------------------------------------------------------------------------------------------
   --  zeropad
   --------------------------------------------------------------------------------------------
   function zeropad (N : Natural; places : Positive) return String
   is
      template : String (1 .. places) := (others => '0');
      myimage  : constant String := trim (N'Img);
      startpos : constant Natural := 1 + places - myimage'Length;
   begin
      template (startpos .. places) := myimage;
      return template;
   end zeropad;

end Core.Strings;
