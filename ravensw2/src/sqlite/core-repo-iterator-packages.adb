--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Event;
with core.Strings;
with core.CommonSQL;
with Core.Repo.Iterator.Populate;
with SQLite;

use Core.Strings;

package body Core.Repo.Iterator.Packages is

   --------------------------------------------------------------------
   --  count
   --------------------------------------------------------------------
   function count (this : in out SQLite_Iterator) return Natural
   is
      result : Natural := 0;
   begin
      if not this.typeset then
         Event.emit_error (NOT_INITIALIZED);
         return 0;
      end if;

      loop
         exit when not SQLite.step_through_statement (this.stmt);
         result := result + 1;
      end loop;
      this.Reset;
      return result;
   end count;


   --------------------------------------------------------------------
   --  Reset
   --------------------------------------------------------------------
   procedure Reset (this : in out SQLite_Iterator) is
   begin
      if not this.typeset then
         Event.emit_error (NOT_INITIALIZED);
         return;
      end if;

      this.counter := 0;
      this.done    := False;
      if not SQLite.reset_statement (this.stmt) then
         Event.emit_notice ("Repo.Iterator.Packages.reset_statement failed");
      end if;
   end Reset;


   --------------------------------------------------------------------
   --  Finalize
   --------------------------------------------------------------------
   overriding
   procedure Finalize (this : in out SQLite_Iterator) is
   begin
      SQLite.finalize_statement (this.stmt);
   end Finalize;


   --------------------------------------------------------------------
   --  search_how
   --------------------------------------------------------------------
   function search_how (this : SQLite_Iterator; field_name : String) return String is
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
            raise illegal_match_style;
      end case;
   end search_how;


   --------------------------------------------------------------------
   --  search_condition
   --------------------------------------------------------------------
   function search_condition (this : SQLite_Iterator) return String
   is
      function make_filter return String;
      function make_filter return String is
      begin
         case this.field is
            when none    => return "";
            when origin  => return this.search_how ("origin");
            when name    => return this.search_how ("name");
            when comment => return this.search_how ("comment");
            when desc    => return this.search_how ("desc");
            when namever => return this.search_how ("name || '-' || version");
         end case;
      end make_filter;

      filter : constant String := make_filter;
   begin
      case this.fsort is
         when none    => return filter & ";";
         when origin  => return filter & " ORDER BY origin;";
         when name    => return filter & " ORDER BY name;";
         when comment => return filter & " ORDER BY comment;";
         when desc    => return filter & " ORDER BY desc;";
         when namever => return filter & " ORDER BY name, version;";
      end case;
   end search_condition;


   --------------------------------------------------------------------
   --  get_sql
   --------------------------------------------------------------------
   function get_sql (this : SQLite_Iterator) return String
   is
      reponame  : constant String := USS (repositories.Element (this.xrepo).name);
      r_url     : constant String := USS (repositories.Element (this.xrepo).url);
      from      : constant String := " FROM packages AS p ";
      url_field : constant String := ", " & SQ (r_url) & " AS repourl";
      selection : constant String :=
        "SELECT p.id, p.origin, p.name, p.version, p.comment, p.name as uniqueid, "
        & "p.prefix, p.desc, p.arch, p.maintainer, p.www, p.licenselogic, p.flatsize, "
        & "p.pkgsize, p.cksum, p.manifestdigest, p.path AS repopath, " & SQ (reponame)
        & " AS dbname";
   begin
      case this.variant is
         when standard_query =>
            return selection & from &
              Database.get_pattern_query (USS (this.pattern), this.mstyle)
              & " ORDER BY name;";
         when search =>
            return selection & url_field & from & this.search_condition;
         when provide =>
            return selection & from &
              "INNER JOIN pkg_provides AS ps ON p.id = ps.package_id "
              & "WHERE ps.provide_id IN (SELECT id from provides WHERE provide = ?1 );";
         when shlib_provide =>
            return selection & from &
              "INNER JOIN pkg_shlibs_provided AS ps ON p.id = ps.package_id "
              & "WHERE ps.shlib_id IN "
              & "(SELECT id FROM shlibs WHERE name BETWEEN ?1 AND ?1 || '.9');";
         when require =>
            return selection & from &
              "INNER JOIN pkg_shlibs_required AS ps ON p.id = ps.package_id "
              & "WHERE ps.shlib_id = (SELECT id FROM shlibs WHERE name=?1);";
         when shlib_require =>
            return selection & from &
              "INNER JOIN pkg_requires AS ps ON p.id = ps.package_id "
              & "WHERE ps.require_id = (SELECT id FROM requires WHERE require=?1);";
      end case;
   end get_sql;


   --------------------------------------------------------------------
   --  initialize_stmt
   --------------------------------------------------------------------
   function initialize_stmt (this : in out SQLite_Iterator) return Action_Result
   is
      sql : constant String := this.get_sql;
   begin
      this.typeset := True;
      this.cycles  := once;
      Event.emit_debug (4, "rdb: running" & SQ (sql));
      if SQLite.prepare_sql (pDB    => repositories.Element (this.xrepo).sqlite_handle,
                             sql    => sql,
                             ppStmt => this.stmt'Access)
      then
         return RESULT_OK;
      end if;

      CommonSQL.ERROR_SQLITE (db      => repositories.Element (this.xrepo).sqlite_handle,
                              srcfile => internal_srcfile,
                              func    => "Repo.Iterator.Packages.initialize_stmt",
                              query   => sql);
      return RESULT_FATAL;
   end initialize_stmt;


   --------------------------------------------------------------------
   --  initialize_as_provide
   --------------------------------------------------------------------
   function initialize_as_provide
     (this     : in out SQLite_Iterator;
      reponame : String;
      pkgname  : String) return Action_Result
   is
   begin
      if this.typeset then
         Event.emit_error ("Iterator already initialized as " & this.variant'Img);
         return RESULT_FATAL;
      end if;

      this.variant := provide;
      this.xrepo   := SUS (reponame);
      if this.initialize_stmt /= RESULT_OK then
         return RESULT_FATAL;
      end if;

      SQLite.bind_string (this.stmt, 1, pkgname);
      return RESULT_OK;
   end initialize_as_provide;


   --------------------------------------------------------------------
   --  initialize_as_shlib_provide
   --------------------------------------------------------------------
   function initialize_as_shlib_provide
     (this     : in out SQLite_Iterator;
      reponame : String;
      pkgname  : String) return Action_Result
   is
   begin
      if this.typeset then
         Event.emit_error ("Iterator already initialized as " & this.variant'Img);
         return RESULT_FATAL;
      end if;

      this.variant := shlib_provide;
      this.xrepo   := SUS (reponame);
      if this.initialize_stmt /= RESULT_OK then
         return RESULT_FATAL;
      end if;

      SQLite.bind_string (this.stmt, 1, pkgname);
      return RESULT_OK;
   end initialize_as_shlib_provide;


   --------------------------------------------------------------------
   --  initialize_as_require
   --------------------------------------------------------------------
   function initialize_as_require
     (this     : in out SQLite_Iterator;
      reponame : String;
      pkgname  : String) return Action_Result
   is
   begin
      if this.typeset then
         Event.emit_error ("Iterator already initialized as " & this.variant'Img);
         return RESULT_FATAL;
      end if;

      this.variant := require;
      this.xrepo   := SUS (reponame);
      if this.initialize_stmt /= RESULT_OK then
         return RESULT_FATAL;
      end if;

      SQLite.bind_string (this.stmt, 1, pkgname);
      return RESULT_OK;
   end initialize_as_require;


   --------------------------------------------------------------------
   --  initialize_as_shlib_require
   --------------------------------------------------------------------
   function initialize_as_shlib_require
     (this     : in out SQLite_Iterator;
      reponame : String;
      pkgname  : String) return Action_Result
   is
   begin
      if this.typeset then
         Event.emit_error ("Iterator already initialized as " & this.variant'Img);
         return RESULT_FATAL;
      end if;

      this.variant := shlib_require;
      this.xrepo   := SUS (reponame);
      if this.initialize_stmt /= RESULT_OK then
         return RESULT_FATAL;
      end if;

      SQLite.bind_string (this.stmt, 1, pkgname);
      return RESULT_OK;
   end initialize_as_shlib_require;


   --------------------------------------------------------------------
   --  initialize_as_search
   --------------------------------------------------------------------
   function initialize_as_search
     (this     : in out SQLite_Iterator;
      reponame : String;
      pattern  : String;
      field    : Match_Field;
      sortby   : Match_Field;
      match    : Database.Match_Behavior) return Action_Result
   is
   begin
      if this.typeset then
         Event.emit_error ("Iterator already initialized as " & this.variant'Img);
         return RESULT_FATAL;
      end if;

      this.variant := search;
      this.xrepo   := SUS (reponame);
      this.field   := field;
      this.fsort   := sortby;
      this.mstyle  := match;
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
   end initialize_as_search;


   --------------------------------------------------------------------
   --  initialize_as_standard_query
   --------------------------------------------------------------------
   function initialize_as_standard_query
     (this     : in out SQLite_Iterator;
      reponame : String;
      pattern  : String;
      match    : Database.Match_Behavior) return Action_Result
   is
   begin
      if this.typeset then
         Event.emit_error ("Iterator already initialized as " & this.variant'Img);
         return RESULT_FATAL;
      end if;

      this.variant := standard_query;
      this.xrepo   := SUS (reponame);
      this.pattern := SUS (pattern);
      this.mstyle  := match;
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
   function Next (this       : in out SQLite_Iterator;
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

      case sqlite_h.sqlite3_step (this.stmt) is

         when sqlite_h.SQLITE_ROW =>
            --  We do not expect pkg_access to be null.  The caller has to allocate
            --  space as necessary
            Populate.populate_pkg (this.stmt, pkg_access);

            if not IsBlank (pkg_access.digest) then
               if not Checksum.checksum_is_valid (pkg_access.digest) then
                  pkg_access.digest := blank;
               end if;
            end if;
            return
              Populate.ensure_sections_loaded
                (db         => repositories.Element (this.xrepo).sqlite_handle,
                 pkg_access => pkg_access,
                 sections   => sections);

         when sqlite_h.SQLITE_DONE =>
            this.done := True;
            case this.cycles is
               when cycled =>
                  if not SQLite.reset_statement (this.stmt) then
                     Event.emit_notice ("Repo.Iterator.Packages.Next.reset_statement failed");
                  end if;
                  return RESULT_OK;
               when once =>
                  return RESULT_END;
               when auto =>
                  --  In freebsd pkg, this is supposed to free itself.
                  --  That's no necessary here, so auto and once are equivalent.
                  return RESULT_END;
            end case;

         when others =>
            CommonSQL.ERROR_SQLITE (db      => repositories.Element (this.xrepo).sqlite_handle,
                                    srcfile => internal_srcfile,
                                    func    => "Repo.Iterator.Packages.Next",
                                    query   => "iterator");
            return RESULT_FATAL;
      end case;
   end Next;

end Core.Repo.Iterator.Packages;
