--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Strings is

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

end Core.Strings;
