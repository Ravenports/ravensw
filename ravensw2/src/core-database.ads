--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


package Core.Database is

   type Match_Behavior is (MATCH_ALL, MATCH_EXACT, MATCH_GLOB, MATCH_REGEX, MATCH_CONDITION);
   type RDB_Type       is (RDB_DB_LOCAL, RDB_DB_REPO);
   type RDB_Source     is (RDB_DEFAULT, RDB_REMOTE, RDB_MAYBE_REMOTE);
   type RDB_Lock_Type  is (RDB_LOCK_READONLY, RDB_LOCK_ADVISORY, RDB_LOCK_EXCLUSIVE);

   type RDB_Mode_Flags is mod 2 ** 3;

   RDB_MODE_EXISTS : constant RDB_Mode_Flags := 0;
   RDB_MODE_READ   : constant RDB_Mode_Flags := 2 ** 0;
   RDB_MODE_WRITE  : constant RDB_Mode_Flags := 2 ** 1;
   RDB_MODE_CREATE : constant RDB_Mode_Flags := 2 ** 2;

   --  By default, MATCH_EXACT and MATCH_REGEX are case sensitive.  This
   --  is modified in many actions according to the value of
   --  CASE_SENSITIVE_MATCH in ravensw.conf and then possibly reset again in
   --  pkg search et al according to command line flags
   procedure set_case_sensitivity (sensitive : Boolean);

   --  Returns true if currently case sensitive
   function case_sensitivity_is_on return Boolean;

   --  The four arguments are mutually exclusive, but the command line parser doesn't
   --  object if more than one set.  Set in priority order exact, glob, regex, condition
   function set_match_behavior
     (request_exact : Boolean := False;
      request_glob  : Boolean := False;
      request_regex : Boolean := False;
      request_condition : Boolean := False) return Match_Behavior;

   --  Common query filter
   function get_pattern_query (pattern : String; match_style : Match_Behavior) return String;

private

   case_sensitivity_setting : Boolean := False;

end Core.Database;
