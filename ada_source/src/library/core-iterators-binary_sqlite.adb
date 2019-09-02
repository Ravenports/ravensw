--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.PkgDB;
with Core.Strings;
with Core.Event;
with Core.Checksum;
with SQLite;

use Core.Strings;

package body Core.Iterators.Binary_sqlite is

   --------------------------------------------------------------------
   --  Free
   --------------------------------------------------------------------
   overriding
   procedure Free (this : in out Iterator_Binary_Sqlite) is
   begin
      SQLite.finalize_statement (this.stmt);
   end Free;


   --------------------------------------------------------------------
   --  Reset
   --------------------------------------------------------------------
   overriding
   procedure Reset (this : in out Iterator_Binary_Sqlite) is
   begin
      this.counter := 0;
      if not SQLite.reset_statement (this.stmt) then
         null;
      end if;
   end Reset;


   --------------------------------------------------------------------
   --  Next
   --------------------------------------------------------------------
   overriding
   function Next (this : in out Iterator_Binary_Sqlite;
                  pkg_ptr : access T_pkg_Access;
                  flags : Load_Flags) return Pkg_Error_Type
   is
      P   : T_pkg renames pkg_ptr.all.all;
      ret : Pkg_Error_Type;
   begin
      if this.counter > 0 and then
        (this.flags and PKGDB_IT_FLAG_ONCE) > 0
      then
         return EPKG_END;
      end if;

      case sqlite_h.sqlite3_step (this.stmt) is
         when sqlite_h.SQLITE_ROW =>
            populate_pkg (this.stmt, pkg_ptr.all);
            if not IsBlank (P.digest) then
               if not Checksum.pkg_checksum_is_valid (P.digest) then
                  P.digest := blank;
               end if;
            end if;
            for load_op in PKG_LOAD_OPS'Range loop
               if (flags and load_on_flag (load_op).flag) > 0 then
                  ret := load_on_flag (load_op).load (this.db, pkg_ptr.all);
                  if ret /= EPKG_OK then
                     return ret;
                  end if;
               end if;
            end loop;
            return EPKG_OK;
         when sqlite_h.SQLITE_DONE =>
            this.counter := this.counter + 1;
            if (this.flags and PKGDB_IT_FLAG_CYCLED) > 0 then
               if not SQLite.reset_statement (this.stmt) then
                  null;
               end if;
               return EPKG_OK;
            else
               if (this.flags and PKGDB_IT_FLAG_AUTO) > 0 then
                  this.Free;
               end if;
               return EPKG_END;
            end if;
         when others =>
            PkgDB.ERROR_SQLITE (this.db, "Core.Iterators.Binary_sqlite.Next", "iterator");
            return EPKG_FATAL;
      end case;

   end Next;


   --------------------------------------------------------------------
   --  get_attribute
   --------------------------------------------------------------------
   function get_attribute (column_name : String) return pkg_attr
   is
      num_aliases : constant := 1;  -- id/rowid
      total_keywords : constant Positive := pkg_attr'Pos (pkg_attr'Last) + 1 + num_aliases;

      subtype keyword_string is String (1 .. 14);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : pkg_attr;
         end record;

      --  Keep in alphabetical order (critical!)
      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("NOTFOUND      ", NOTFOUND),
         ("arch          ", PKG_ARCH),
         ("automatic     ", PKG_AUTOMATIC),
         ("cksum         ", PKG_CKSUM),
         ("comment       ", PKG_COMMENT),
         ("dbname        ", PKG_REPONAME),
         ("dep_formula   ", PKG_DEP_FORMULA),
         ("desc          ", PKG_DESC),
         ("flatsize      ", PKG_FLATSIZE),
         ("id            ", PKG_ROWID),
         ("licenselogic  ", PKG_LICENSE_LOGIC),
         ("locked        ", PKG_LOCKED),
         ("maintainer    ", PKG_MAINTAINER),
         ("manifestdigest", PKG_DIGEST),
         ("message       ", PKG_MESSAGE),
         ("name          ", PKG_NAME),
         ("oldflatsize   ", PKG_OLD_FLATSIZE),
         ("oldversion    ", PKG_OLD_VERSION),
         ("origin        ", PKG_ORIGIN),
         ("pkgsize       ", PKG_PKGSIZE),
         ("prefix        ", PKG_PREFIX),
         ("repopath      ", PKG_REPOPATH),
         ("repourl       ", PKG_REPOURL),
         ("rowid         ", PKG_ROWID),
         ("time          ", PKG_TIME),
         ("uniqueid      ", PKG_UNIQUEID),
         ("version       ", PKG_VERSION),
         ("vital         ", PKG_VITAL),
         ("www           ", PKG_WWW)
        );

      bandolier : keyword_string := (others => ' ');
      Low       : Natural := all_keywords'First;
      High      : Natural := all_keywords'Last;
      Mid       : Natural;
   begin
      if column_name'Length > keyword_string'Length or else
        column_name'Length < 2
      then
         return NOTFOUND;
      end if;

      bandolier (1 .. column_name'Length) := column_name;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return NOTFOUND;
   end get_attribute;


   --------------------------------------------------------------------
   --  clear_pkg_data
   --------------------------------------------------------------------
   procedure clear_pkg_data (pkg_access : T_pkg_Access)
   is
      P : T_pkg renames pkg_access.all;
   begin
      P.id        := T_pkg_id'First;
      P.direct    := False;
      P.locked    := False;
      P.automatic := False;
      P.vital     := False;

      P.name        := blank;
      P.origin      := blank;
      P.version     := blank;
      P.old_version := blank;
      P.maintainer  := blank;
      P.www         := blank;
      P.arch        := blank;
      P.abi         := blank;
      P.uid         := blank;
      P.digest      := blank;
      P.old_digest  := blank;
      P.prefix      := blank;
      P.comment     := blank;
      P.desc        := blank;
      P.sum         := blank;
      P.repopath    := blank;
      P.reponame    := blank;
      P.repourl     := blank;
      P.reason      := blank;
      P.dep_formula := blank;
      P.rootpath    := blank;

      P.pkgsize      := T_pkg_size'First;
      P.flatsize     := T_pkg_size'First;
      P.old_flatsize := T_pkg_size'First;
      P.timestamp    := T_pkg_timestamp'First;

      P.depends.Clear;
      P.rdepends.Clear;
      P.messages.Clear;

      --  later: scripts, licenselogic

   end clear_pkg_data;


   --------------------------------------------------------------------
   --  populate_pkg
   --------------------------------------------------------------------
   procedure populate_pkg (stmt : sqlite_h.sqlite3_stmt_Access;
                           pkg_access : T_pkg_Access)
   is
      function get_text (col_index : Natural) return Text;
      function get_boolean (col_index : Natural) return Boolean;
      function get_pkgsize (col_index : Natural) return T_pkg_size;
      function get_pkgid (col_index : Natural) return T_pkg_id;
      function get_timestamp (col_index : Natural) return T_pkg_timestamp;
      function get_message (col_index : Natural) return T_message;
      function get_liclogic (col_index : Natural) return T_licenselogic;

      P : T_pkg renames pkg_access.all;

      function get_text (col_index : Natural) return Text is
      begin
         return SUS (SQLite.retrieve_string (stmt, col_index));
      end get_text;

      function get_boolean (col_index : Natural) return Boolean
      is
         use type SQLite.sql_int64;
         result : SQLite.sql_int64;
      begin
         result := SQLite.retrieve_integer (stmt, col_index);
         return (result > 0);
      end get_boolean;

      function get_pkgsize (col_index : Natural) return T_pkg_size is
      begin
         return T_pkg_size (SQLite.retrieve_integer (stmt, col_index));
      end get_pkgsize;

      function get_pkgid (col_index : Natural) return T_pkg_id is
      begin
         return T_pkg_id (SQLite.retrieve_integer (stmt, col_index));
      end get_pkgid;

      function get_timestamp (col_index : Natural) return T_pkg_timestamp is
      begin
         return T_pkg_timestamp (SQLite.retrieve_integer (stmt, col_index));
      end get_timestamp;

      function get_message (col_index : Natural) return T_message
      is
         result : T_message;
      begin
         result.contents := get_text (col_index);
         return result;
      end get_message;

      function get_liclogic (col_index : Natural) return T_licenselogic
      is
         r64  : SQLite.sql_int64 := SQLite.retrieve_integer (stmt, col_index);
         AOR  : constant SQLite.sql_int64 := SQLite.sql_int64 (Character'Pos ('|'));
         AAND : constant SQLite.sql_int64 := SQLite.sql_int64 (Character'Pos ('&'));
      begin
         case r64 is
            when AOR    => return LICENSE_OR;
            when AAND   => return LICENSE_AND;
            when 1      => return LICENSE_SINGLE;
            when others =>
               Event.pkg_emit_notice
                 (SUS ("Invalid license logic value " & int2str (Integer (r64)) &
                    ", defaulting to single license"));
               return LICENSE_SINGLE;
         end case;
      end get_liclogic;

   begin
      clear_pkg_data (pkg_access);
      for icol in 0 .. SQLite.get_number_of_columns (stmt) - 1 loop
         declare
            colname  : constant String := SQLite.get_column_name (stmt, icol);
            datatype : pkg_attr := get_attribute (colname);
         begin
            case datatype is
               when PKG_ABI           => P.abi := get_text (icol);
               when PKG_CKSUM         => P.sum := get_text (icol);
               when PKG_COMMENT       => P.comment := get_text (icol);
               when PKG_REPONAME      => P.reponame := get_text (icol);
               when PKG_DESC          => P.desc := get_text (icol);
               when PKG_MAINTAINER    => P.maintainer := get_text (icol);
               when PKG_DIGEST        => P.digest := get_text (icol);
               when PKG_NAME          => P.name := get_text (icol);
               when PKG_OLD_VERSION   => P.old_version := get_text (icol);
               when PKG_ORIGIN        => P.origin := get_text (icol);
               when PKG_PREFIX        => P.prefix := get_text (icol);
               when PKG_REPOPATH      => P.repopath := get_text (icol);
               when PKG_REPOURL       => P.repourl := get_text (icol);
               when PKG_UNIQUEID      => P.uid := get_text (icol);
               when PKG_VERSION       => P.version := get_text (icol);
               when PKG_WWW           => P.www := get_text (icol);
               when PKG_DEP_FORMULA   => P.dep_formula := get_text (icol);
               when PKG_MESSAGE       => P.messages.Append (get_message (icol));

               when PKG_AUTOMATIC     => P.automatic := get_boolean (icol);
               when PKG_LOCKED        => P.locked := get_boolean (icol);
               when PKG_VITAL         => P.vital := get_boolean (icol);

               when PKG_PKGSIZE       => P.pkgsize := get_pkgsize (icol);
               when PKG_OLD_FLATSIZE  => P.old_flatsize := get_pkgsize (icol);
               when PKG_FLATSIZE      => P.flatsize := get_pkgsize (icol);
               when PKG_ROWID         => P.id := get_pkgid (icol);
               when PKG_TIME          => P.timestamp := get_timestamp (icol);
               when PKG_LICENSE_LOGIC => P.licenselogic := get_liclogic (icol);

                  --  Not seen in query
               when PKG_MTREE => null;
               when PKG_ARCH  => null;
               when PKG_REASON => null;
               when PKG_ANNOTATIONS => null;
               when PKG_OLD_DIGEST => null;
               when PKG_NUM_FIELDS => null;

               when NOTFOUND =>
                  Event.pkg_emit_error (SUS ("populate_pkg(): unknown column " & colname));
            end case;
         end;
      end loop;
   end populate_pkg;


end Core.Iterators.Binary_sqlite;
