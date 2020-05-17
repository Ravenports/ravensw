--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers;

package Core.Strings is

   package CON renames Ada.Containers;

   blank : constant Text := SU.Null_Unbounded_String;

   --  converters : Text <==> String
   function USS (US : Text)   return String;
   function SUS (S  : String) return Text;

   --  shorthand for index
   function contains (S : String; fragment : String) return Boolean;
   function contains (US : Text; fragment : String) return Boolean;

   --  Return True if S terminates with fragment exactly
   function trails (S  : String; fragment : String) return Boolean;
   function trails (US : Text;   fragment : String) return Boolean;

   --  Return True if S leads with fragment exactly
   function leads (S  : String; fragment : String) return Boolean;
   function leads (US : Text;   fragment : String) return Boolean;

   --  Return half of a string split by separator
   function part_1 (S : String; separator : String := "/") return String;
   function part_2 (S : String; separator : String := "/") return String;

   --  True if the string is zero length
   function IsBlank (US : Text)   return Boolean;
   function IsBlank (S  : String) return Boolean;

   --  Replace single character with another single character (all found)
   function replace_all (S : String; reject, shiny : Character) return String;

   --  unpadded numeric image
   function int2str  (A : Integer) return String;
   function int2text (A : Integer) return Text;

   --  Replace substring with another string
   function replace_substring (US : Text;
                               old_string : String;
                               new_string : String) return Text;

   --  Returns number of instances of a given character in a given string
   function count_char (S : String; focus : Character) return Natural;

   --  Escape " and \ characters by prefacing them with a \ character
   function json_escape (S : String) return String;

   --  Given a single line (presumably no line feeds) with data separated by <delimited>,
   --  return the field given by field_number (starts counting at 1).
   function specific_field
     (S            : String;
      field_number : Positive;
      delimiter    : String := " ") return String;

   --  Convert to uppercase
   function uppercase (US : Text)   return Text;
   function uppercase (S  : String) return String;

   --  Convert to lowercase
   function lowercase (US : Text)   return Text;
   function lowercase (S  : String) return String;

   --  Print octal number given base 10 with optional zero-padding
   function octal (number : Natural; places : Positive; zero_pad : Boolean) return String;

   --  Returns index of first character of fragment (0 if not found)
   function start_index (S : String; fragment : String) return Natural;

      --  True if strings are identical
   function equivalent (A, B : Text) return Boolean;
   function equivalent (A : Text; B : String) return Boolean;

   --  Used for mapped containers
   function map_hash (key : Text) return CON.Hash_Type;

   --  Create a string with {places} length and put S at the first index.
   --  Truncate from back if S is longer than places.
   function pad_right (S : String; places : Positive) return String;

   --  Create a string with {places} length and put S at the index that lines up S'Last with
   --  the last character of the result.  Truncate from front if S is longer than places.
   function pad_left (S : String; places : Positive) return String;

   --  Head (keep all but last delimiter and field)
   function head (US : Text;   delimiter : Text)   return Text;
   function head (S  : String; delimiter : String) return String;

   --  Tail (keep only last field)
   function tail (US : Text;   delimiter : Text)   return Text;
   function tail (S  : String; delimiter : String) return String;

   --  Return input surrounded by double quotation marks
   function DQ (txt : String) return String;

   --  Return input surrounded by single quotation marks
   function SQ (txt : String) return String;

   --  Trim both sides
   function trim (S : String) return String;

      --  Numeric image with left-padded zeros
   function zeropad (N : Natural; places : Positive) return String;

end Core.Strings;
