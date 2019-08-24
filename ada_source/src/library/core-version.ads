--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Strings;  use Core.Strings;

package Core.Version is

   subtype cmp_result is Integer range -1 .. +1;

   type version_breakdown is
      record
         version  : Text;
         revision : Natural;
         epoch    : Natural;
      end record;

   --
   --  split_version(pkgname, endname, epoch, revision) returns a pointer to
   --  the version portion of a package name and the two special components.
   --
   --  Syntax is:  ${PORTNAME}-${PORTVERSION}[_${PORTREVISION}][,${PORTEPOCH}]
   --
   --  Written by Oliver Eikemeier
   --  Based on work of Jeremy D. Lea.
   --
   function split_version (pkgname : String) return version_breakdown;

   --
   --  version_cmp(pkg1, pkg2) returns -1, 0 or 1 depending on if the version
   --  components of pkg1 is less than, equal to or greater than pkg2. No
   --  comparison of the basenames is done.
   --
   --  The port version is defined by:
   --  ${PORTVERSION}[_${PORTREVISION}][,${PORTEPOCH}]
   --  ${PORTEPOCH} supersedes ${PORTVERSION} supersedes ${PORTREVISION}.
   --  See the commit log for revision 1.349 of ports/Mk/bsd.port.mk
   --  for more information.
   --
   --  The epoch and revision are defined to be a single number, while the rest
   --  of the version should conform to the porting guidelines. It can contain
   --  multiple components, separated by a period, including letters.
   --
   --  The tests allow for significantly more latitude in the version numbers
   --  than is allowed in the guidelines. No point in enforcing them here.
   --  That's what portlint is for.
   --
   --  Jeremy D. Lea.
   --  reimplemented by Oliver Eikemeier
   --
   function pkg_version_cmp (pkg1, pkg2 : String) return cmp_result;

private

   type vc_int64 is range -(2**63) .. +(2**63 - 1);

   --
   --  PORTVERSIONs are composed of components separated by dots. A component
   --  consists of a version number, a letter and a patchlevel number. This does
   --  not conform to the porter's handbook, but let us formulate rules that
   --  fit the current practice and are far simpler than to make decisions
   --  based on the order of netters and lumbers. Besides, people use versions
   --  like 10b2 in the ports...

   type version_component is
      record
         n  : vc_int64 := 0;
         pl : vc_int64 := 0;
         a  : Integer  := 0;
      end record;

   --
   --  get_component(position, component) gets the value of the next component
   --  (number - letter - number triple) and returns a pointer to the next character
   --  after any leading separators
   --
   --  - components are separated by dots
   --  - characters !~ [a-zA-Z0-9.+*] are treated as separators
   --    (1.0:2003.09.16 = 1.0.2003.09.16), this may not be what you expect:
   --    1.0.1:2003.09.16 < 1.0:2003.09.16
   --  - consecutive separators are collapsed (10..1 = 10.1)
   --  - missing separators are inserted, essentially
   --    letter number letter => letter number . letter (10a1b2 = 10a1.b2)
   --  - missing components are assumed to be equal to 0 (10 = 10.0 = 10.0.0)
   --  - the letter sort order is: [none], a, b, ..., z; numbers without letters
   --    sort first (10 < 10a < 10b)
   --  - missing version numbers (in components starting with a letter) sort as -1
   --    (a < 0, 10.a < 10)
   --  - a separator is inserted before the special strings "pl", "alpha", "beta",
   --    "pre" and "rc".
   --  - "pl" sorts before every other letter, "alpha", "beta", "pre" and "rc"
   --    sort as a, b, p and r. (10alpha = 10.a < 10, but 10 < 10a; pl11 < alpha3
   --    < 0.1beta2 = 0.1.b2 < 0.1)
   --  - other strings use only the first letter for sorting, case is ignored
   --    (1.d2 = 1.dev2 = 1.Development2)
   --  - The special component `*' is guaranteed to be the smallest possible
   --    component (2.* < 2pl1 < 2alpha3 < 2.9f7 < 3.*)
   --  - components separated by `+' are handled by version_cmp below
   --
   --  Oliver Eikemeier
   --

   type stage_type is
      record
         name    : String (1 .. 5);
         namelen : Natural;
         value   : Natural;
      end record;

   stage : constant array (1 .. 5) of stage_type :=
     (
      ("pl   ", 2, 0),
      ("alpha", 5, 1),
      ("beta ", 4, 2),
      ("pre  ", 3, Character'Pos ('p') - Character'Pos ('a') + 1),
      ("rc   ", 2, Character'Pos ('r') - Character'Pos ('a') + 1)
     );

   function get_component
     (full_version : in String;
      position     : in out Integer) return version_component;

   --  Return True if x is '0' .. '9'
   function is_digit (x : Character) return Boolean;

   --  Return True if x is 'A' .. 'Z' or 'a' .. 'z'
   function is_alpha (x : Character) return Boolean;

   --  Returns lowercase of 'A' .. 'Z' others returns x unchanged
   function to_lower (x : Character) return Character;

   --  When given a position starting with a digit, extract all characters from
   --  there representing a number and then convert it.  Update the position accordingly.
   --  '+' and '-' not supported (there are no negative port versions I hope).
   --  '-' is considered a hyphen anyway, and '+' is considered part of the version name
   function pull_leading_number
     (full_version : in String;
      position     : in out Integer) return vc_int64;

end Core.Version;
