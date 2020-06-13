--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Event;
with Core.Strings;
with Core.CommonSQL;
with Core.Repo.Iterator.Populate;
with Core.Checksum;

use Core.Strings;

package body Core.Database.Iterator is

   package POP renames Core.Repo.Iterator.Populate;

   --------------------------------------------------------------------
   --  count
   --------------------------------------------------------------------
   function count (this : in out DB_SQLite_Iterator) return Natural
   is
      result : Natural := 0;
   begin
      if not this.typeset then
         Event.emit_error (NOT_INITIALIZED);
         return 0;
      end if;

      loop
         exit when not SQLite.step_to_another_row (this.stmt);
         result := result + 1;
      end loop;
      this.Reset;
      return result;
   end count;


   --------------------------------------------------------------------
   --  Reset
   --------------------------------------------------------------------
   procedure Reset (this : in out DB_SQLite_Iterator) is
   begin
      if not this.typeset then
         Event.emit_error (NOT_INITIALIZED);
         return;
      end if;

      this.counter := 0;
      this.done    := False;
      if not SQLite.reset_statement (this.stmt) then
         Event.emit_notice ("Database.Iterator.reset_statement failed");
      end if;
   end Reset;


   --------------------------------------------------------------------
   --  Finalize
   --------------------------------------------------------------------
   overriding
   procedure Finalize (this : in out DB_SQLite_Iterator) is
   begin
      SQLite.finalize_statement (this.stmt);
   end Finalize;


   --------------------------------------------------------------------
   --  search_how
   --------------------------------------------------------------------
   function search_how (this : DB_SQLite_Iterator; field_name : String) return String is
   begin
      case this.mstyle is
         when Database.MATCH_ALL   => return "";
         when Database.MATCH_GLOB  => return "WHERE " & field_name & " GLOB ?1";
         when Database.MATCH_REGEX => return "WHERE " & field_name & " REGEXP ?1";
         when Database.MATCH_EXACT =>
            if Database.case_sensitivity_is_on then
               return "WHERE " & field_name & " = ?1";
            else
               return "WHERE " & field_name & " = ?1 COLLATE NOCASE";
            end if;
         when Database.MATCH_CONDITION =>
            raise db_illegal_match_style;
      end case;
   end search_how;


   --------------------------------------------------------------------
   --  search_condition
   --------------------------------------------------------------------
   function search_condition (this : DB_SQLite_Iterator) return String
   is
      function make_filter return String;
      function make_filter return String is
      begin
         case this.field is
            when none    => return "";
            when origin  => return this.search_how ("p.origin");
            when name    => return this.search_how ("p.name");
            when comment => return this.search_how ("p.comment");
            when desc    => return this.search_how ("p.desc");
            when namever => return this.search_how ("p.name || '-' || p.version");
         end case;
      end make_filter;

      filter : constant String := make_filter;
   begin
      case this.fsort is
         when none    => return filter;
         when origin  => return filter & " ORDER BY p.origin";
         when name    => return filter & " ORDER BY p.name";
         when comment => return filter & " ORDER BY p.comment";
         when desc    => return filter & " ORDER BY p.desc";
         when namever => return filter & " ORDER BY p.name, p.version";
      end case;
   end search_condition;


   --------------------------------------------------------------------
   --  initialize_stmt
   --------------------------------------------------------------------
   function initialize_stmt (this : in out DB_SQLite_Iterator) return Action_Result is
   begin
      this.typeset := True;
      this.cycles  := once;

      declare
         sql : constant String := this.get_sql;
      begin
         Event.emit_debug
            (4, "local: initializing " & pad_right (this.variant'Img, 14) & " > " &  DQ (sql));
         if SQLite.prepare_sql (pDB  => this.dbconn,
                                sql  => sql,
                                stmt => this.stmt)
         then
            return RESULT_OK;
         end if;
         CommonSQL.ERROR_SQLITE (db      => this.dbconn,
                                 srcfile => internal_srcfile,
                                 func    => "Database.Iterator.initialize_stmt",
                                 query   => sql);
         return RESULT_FATAL;
      end;
   end initialize_stmt;


   --------------------------------------------------------------------
   --  get_sql
   --------------------------------------------------------------------
   function get_sql (this : DB_SQLite_Iterator) return String
   is
      function limit_wrapper (sql : String) return String;

      from      : constant String := " FROM packages AS p ";
      selection : constant String :=
        "SELECT p.id, p.origin, p.name, p.version, p.comment, p.name as uniqueid, "
        & "p.prefix, p.desc, p.arch, p.maintainer, p.www, p.licenselogic, p.flatsize, "
        & "p.manifestdigest, p.message, p.automatic, p.locked, p.time, p.vital";

      function limit_wrapper (sql : String) return String is
      begin
         if this.limit1 then
            return sql & " LIMIT 1;";
         else
            return sql & ";";
         end if;
      end limit_wrapper;
   begin
      case this.variant is
         when standard_query =>
            return limit_wrapper
              (selection & from
               & Database.get_pattern_query (USS (this.pattern), this.mstyle)
               & " ORDER BY p.name");
      end case;
   end get_sql;


   --------------------------------------------------------------------
   --  initialize_as_standard_query
   --------------------------------------------------------------------
   function initialize_as_standard_query
     (this     : in out DB_SQLite_Iterator;
      conn     : Database.RDB_Connection;
      pattern  : String;
      match    : Database.Match_Behavior;
      just_one : Boolean) return Action_Result
   is
   begin
      if this.typeset then
         Event.emit_error ("Iterator already initialized as " & this.variant'Img);
         return RESULT_FATAL;
      end if;

      this.variant := standard_query;
      this.pattern := SUS (pattern);
      this.mstyle  := match;
      this.limit1  := just_one;
      this.dbconn  := conn.sqlite;
      if this.initialize_stmt /= RESULT_OK then
         return RESULT_FATAL;
      end if;

      case this.mstyle is
         when Database.MATCH_ALL
            | Database.MATCH_CONDITION => null;
         when others =>
            SQLite.bind_string (this.stmt, 1, pattern);
      end case;
      return RESULT_OK;
   end initialize_as_standard_query;


   --------------------------------------------------------------------
   --  Next
   --------------------------------------------------------------------
   function Next (this       : in out DB_SQLite_Iterator;
                  pkg_access : Pkgtypes.A_Package_Access;
                  sections   : Pkgtypes.Package_Load_Flags := (others => True))
                  return Action_Result is
   begin
      if not this.typeset then
         Event.emit_error (NOT_INITIALIZED);
         return RESULT_FATAL;
      end if;

      if this.done and then (this.cycles = once) then
         return RESULT_END;
      end if;

      case SQLite.step (this.stmt) is

         when SQLite.row_present =>
            --  We do not expect pkg_access to be null.  The caller has to allocate
            --  space as necessary
            POP.populate_pkg (this.stmt, pkg_access);

            if not IsBlank (pkg_access.digest) then
               if not Checksum.checksum_is_valid (pkg_access.digest) then
                  pkg_access.digest := blank;
               end if;
            end if;
            return
              POP.ensure_sections_loaded
                (db         => this.dbconn,
                 pkg_access => pkg_access,
                 sections   => sections);

         when SQLite.no_more_data =>
            this.done := True;
            case this.cycles is
               when cycled =>
                  if not SQLite.reset_statement (this.stmt) then
                     Event.emit_notice ("Database.Iterator.Next.reset_statement failed");
                  end if;
                  return RESULT_OK;
               when once =>
                  return RESULT_END;
               when auto =>
                  --  In freebsd pkg, this is supposed to free itself.
                  --  That's no necessary here, so auto and once are equivalent.
                  return RESULT_END;
            end case;

         when SQLite.something_else =>
            CommonSQL.ERROR_SQLITE (db      => this.dbconn,
                                    srcfile => internal_srcfile,
                                    func    => "Database.Iterator.Next",
                                    query   => "iterator");
            return RESULT_FATAL;
      end case;
   end Next;

end Core.Database.Iterator;
