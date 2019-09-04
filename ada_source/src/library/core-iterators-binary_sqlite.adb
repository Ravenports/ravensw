--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.PkgDB;
with Core.Strings;
with Core.Event;
with Core.Checksum;
with Core.Deps;
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

      P.flags       := 0;

      P.pkgsize      := T_pkg_size'First;
      P.flatsize     := T_pkg_size'First;
      P.old_flatsize := T_pkg_size'First;
      P.timestamp    := T_pkg_timestamp'First;
      P.licenselogic := T_licenselogic'First;

      P.depends.Clear;
      P.rdepends.Clear;
      P.messages.Clear;
      P.categories.Clear;
      P.licenses.Clear;
      P.users.Clear;
      P.groups.Clear;
      P.shlibs_reqd.Clear;
      P.shlibs_prov.Clear;
      P.provides.Clear;
      P.requires.Clear;
      P.conflicts.Clear;
      P.options.Clear;
      P.annotations.Clear;
      P.dirs.Clear;
      P.files.Clear;
      P.config_files.Clear;

      for x in pkg_script_type'Range loop
         P.scripts (x) := blank;
      end loop;

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


   --------------------------------------------------------------------
   --  load_val
   --------------------------------------------------------------------
   function load_val
     (db         : sqlite_h.sqlite3_Access;
      pkg_access : T_pkg_Access;
      operation  : PKG_LOAD_OPS;
      sql        : String)
      return Pkg_Error_Type
   is
      stmt : aliased sqlite_h.sqlite3_stmt_Access;
   begin
      if (pkg_access.flags and load_on_flag (operation).flag) > 0 then
         return EPKG_OK;
      end if;

      Event.pkg_debug (4, "Pkgdb: running '" & sql & "'");
      if not SQLite.prepare_sql (db, sql, stmt'Access) then
         PkgDB.ERROR_SQLITE (db, "load_val", sql);
         return EPKG_FATAL;
      end if;

      SQLite.bind_integer (stmt, 1, SQLite.sql_int64 (pkg_access.id));

      declare
         problem : Boolean;
         result  : Pkg_Error_Type;
      begin
         loop
            exit when not SQLite.step_through_statement (stmt, problem);
            result := load_val_operation (operation) (stmt, pkg_access);
            if result /= EPKG_OK then
               problem := True;
               exit;
            end if;
         end loop;

         if problem then
            case operation is
               when PKG_LOAD_LICENSES        => pkg_access.licenses.Clear;
               when PKG_LOAD_CATEGORIES      => pkg_access.categories.Clear;
               when PKG_LOAD_USERS           => pkg_access.users.Clear;
               when PKG_LOAD_GROUPS          => pkg_access.groups.Clear;
               when PKG_LOAD_SHLIBS_REQUIRED => pkg_access.shlibs_reqd.Clear;
               when PKG_LOAD_SHLIBS_PROVIDED => pkg_access.shlibs_prov.Clear;
               when PKG_LOAD_PROVIDES        => pkg_access.provides.Clear;
               when PKG_LOAD_REQUIRES        => pkg_access.requires.Clear;
               when PKG_LOAD_CONFLICTS       => pkg_access.conflicts.Clear;
               when PKG_LOAD_ANNOTATIONS     => pkg_access.annotations.Clear;
               when PKG_LOAD_DEPS            => null;
               when PKG_LOAD_DEP_FORMULA     => null;
               when PKG_LOAD_RDEPS           => pkg_access.rdepends.Clear;
               when PKG_LOAD_DIRS            => pkg_access.dirs.Clear;
               when PKG_LOAD_OPTIONS         => pkg_access.options.Clear;
               when PKG_LOAD_FILES           => pkg_access.files.Clear;
               when PKG_LOAD_CONFIG_FILES    => pkg_access.config_files.Clear;
               when PKG_LOAD_SCRIPTS         =>
                  for x in pkg_script_type'Range loop
                     pkg_access.scripts (x) := blank;
                  end loop;
            end case;
            PkgDB.ERROR_SQLITE (db, "load_val (step)", sql);
            return EPKG_FATAL;
         end if;
      end;

      SQLite.finalize_statement (stmt);

      pkg_access.flags := pkg_access.flags or load_on_flag (operation).flag;

      return EPKG_OK;
   end load_val;


   --------------------------------------------------------------------
   --  add_license
   --------------------------------------------------------------------
   function add_license (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                         return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.licenses.Append (data);
      return EPKG_OK;
   end add_license;


   --------------------------------------------------------------------
   --  add_category
   --------------------------------------------------------------------
   function add_category (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                         return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.categories.Append (data);
      return EPKG_OK;
   end add_category;


   --------------------------------------------------------------------
   --  add_user
   --------------------------------------------------------------------
   function add_user (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                         return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.users.Append (data);
      return EPKG_OK;
   end add_user;


   --------------------------------------------------------------------
   --  add_group
   --------------------------------------------------------------------
   function add_group (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                       return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.groups.Append (data);
      return EPKG_OK;
   end add_group;


   --------------------------------------------------------------------
   --  add_provides
   --------------------------------------------------------------------
   function add_provides (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                          return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.provides.Append (data);
      return EPKG_OK;
   end add_provides;


   --------------------------------------------------------------------
   --  add_requires
   --------------------------------------------------------------------
   function add_requires (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                          return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.requires.Append (data);
      return EPKG_OK;
   end add_requires;


   --------------------------------------------------------------------
   --  add_shlib_reqd
   --------------------------------------------------------------------
   function add_shlib_reqd (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                            return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.shlibs_reqd.Append (data);
      return EPKG_OK;
   end add_shlib_reqd;


   --------------------------------------------------------------------
   --  add_shlib_prov
   --------------------------------------------------------------------
   function add_shlib_prov (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                            return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.shlibs_prov.Append (data);
      return EPKG_OK;
   end add_shlib_prov;


   --------------------------------------------------------------------
   --  add_conflict
   --------------------------------------------------------------------
   function add_conflict (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                            return Pkg_Error_Type
   is
      unique_id : Text := SUS (SQLite.retrieve_string (stmt, 0));
      conflict : T_pkg_conflict;
   begin
      --  silently ignore duplicates in case of conflicts
      if not pkg_access.conflicts.Contains (unique_id) then
         Event.pkg_debug (3, "Pkg: add a new conflict origin: " & USS (pkg_access.uid) &
                            " with " & USS (unique_id));
         --  don't set conflict.digest or conflict.contype right now
         conflict.uid := unique_id;
         pkg_access.conflicts.Insert (unique_id, conflict);
      end if;
      return EPKG_OK;
   end add_conflict;


   --------------------------------------------------------------------
   --  add_annotation
   --------------------------------------------------------------------
   function add_annotation (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                            return Pkg_Error_Type
   is
      name  : Text := SUS (SQLite.retrieve_string (stmt, 0));
      value : Text := SUS (SQLite.retrieve_string (stmt, 1));
   begin
      if not pkg_access.annotations.Contains (name) then
         pkg_access.annotations.Insert (name, value);
      end if;
      return EPKG_OK;
   end add_annotation;


   --------------------------------------------------------------------
   --  add_directory
   --------------------------------------------------------------------
   function add_directory (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                           return Pkg_Error_Type
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      return pkg_adddir_attr (pkg_access       => pkg_access,
                              path             => data,
                              uname            => blank,
                              gname            => blank,
                              perm             => 0,
                              fflags           => 0,
                              check_duplicates => False);
   end add_directory;


   --------------------------------------------------------------------
   --  add_scripts
   --------------------------------------------------------------------
   function add_scripts (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                         return Pkg_Error_Type
   is
      use type SQLite.sql_int64;

      script : Text := SUS (SQLite.retrieve_string (stmt, 0));
      type_index : SQLite.sql_int64 := SQLite.retrieve_integer (stmt, 1);
   begin
      if type_index < 0 or else
        type_index > SQLite.sql_int64 (pkg_script_type'Pos (pkg_script_type'Last))
      then
         Event.pkg_emit_error (SUS ("add_scripts(): script type out of range"));
         return EPKG_FATAL;
      end if;

      pkg_access.scripts (pkg_script_type'Val (type_index)) := script;
      return EPKG_OK;
   end add_scripts;


   --------------------------------------------------------------------
   --  add_rdeps
   --------------------------------------------------------------------
   function add_rdeps (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                       return Pkg_Error_Type
   is
      name    : Text := SUS (SQLite.retrieve_string (stmt, 0));
      origin  : Text := SUS (SQLite.retrieve_string (stmt, 1));
      version : Text := SUS (SQLite.retrieve_string (stmt, 2));
   begin
      return pkg_addrdep (pkg_access => pkg_access,
                          name       => name,
                          origin     => origin,
                          version    => version,
                          locked     => False);
   end add_rdeps;


   --------------------------------------------------------------------
   --  add_config_files
   --------------------------------------------------------------------
   function add_config_files (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                              return Pkg_Error_Type
   is
      path    : Text := SUS (SQLite.retrieve_string (stmt, 0));
      content : Text := SUS (SQLite.retrieve_string (stmt, 1));
   begin
      return pkg_addconfig_file (pkg_access, path, content);
   end add_config_files;


   --------------------------------------------------------------------
   --  add_files
   --------------------------------------------------------------------
   function add_files (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                       return Pkg_Error_Type
   is
      path   : Text := SUS (SQLite.retrieve_string (stmt, 0));
      sha256 : Text := SUS (SQLite.retrieve_string (stmt, 1));
   begin
      return pkg_addfile_attr (pkg_access       => pkg_access,
                               path             => path,
                               uname            => blank,
                               gname            => blank,
                               perm             => 0,
                               fflags           => 0,
                               sum              => sha256,
                               check_duplicates => False);
   end add_files;


   --------------------------------------------------------------------
   --  add_options
   --------------------------------------------------------------------
   function add_options (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                         return Pkg_Error_Type
   is
      name  : Text := SUS (SQLite.retrieve_string (stmt, 0));
      value : Text := SUS (SQLite.retrieve_string (stmt, 1));
   begin
      return pkg_addoption (pkg_access, name, value);
   end add_options;


   --------------------------------------------------------------------
   --  add_deps
   --------------------------------------------------------------------
   function add_deps (stmt : sqlite_h.sqlite3_stmt_Access; pkg_access : T_pkg_Access)
                      return Pkg_Error_Type
   is
      name    : Text := SUS (SQLite.retrieve_string (stmt, 0));
      origin  : Text := SUS (SQLite.retrieve_string (stmt, 1));
      version : Text := SUS (SQLite.retrieve_string (stmt, 2));
   begin
      return pkg_adddep (pkg_access => pkg_access,
                          name       => name,
                          origin     => origin,
                          version    => version,
                          locked     => False);
   end add_deps;


   --------------------------------------------------------------------
   --  pkgdb_load_categories
   --------------------------------------------------------------------
   function pkgdb_load_categories (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                   return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT name" &
        "  FROM pkg_categories, categories AS c" &
        "  WHERE package_id = ?1" &
        "    AND category_id = c.id" &
        "  ORDER by name DESC";
   begin
      return load_val (db, pkg_access, PKG_LOAD_CATEGORIES, sql);
   end pkgdb_load_categories;


   --------------------------------------------------------------------
   --  pkgdb_load_license
   --------------------------------------------------------------------
   function pkgdb_load_licenses (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT ifnull(group_concat(name, ', '), '') AS name" &
        "  FROM pkg_licenses, licenses AS l" &
        "  WHERE package_id = ?1" &
        "    AND license_id = l.id" &
        "  ORDER by name DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_LICENSES, sql);
   end pkgdb_load_licenses;


   --------------------------------------------------------------------
   --  pkgdb_load_users
   --------------------------------------------------------------------
   function pkgdb_load_users (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT users.name" &
        "  FROM pkg_users, users" &
        "  WHERE package_id = ?1" &
        "    AND user_id = users.id" &
        "  ORDER by name DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_USERS, sql);
   end pkgdb_load_users;


   --------------------------------------------------------------------
   --  pkgdb_load_groups
   --------------------------------------------------------------------
   function pkgdb_load_groups (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT groups.name" &
        "  FROM pkg_groups, groups" &
        "  WHERE package_id = ?1" &
        "    AND group_id = groups.id" &
        "  ORDER by name DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_GROUPS, sql);
   end pkgdb_load_groups;


   --------------------------------------------------------------------
   --  pkgdb_load_provides
   --------------------------------------------------------------------
   function pkgdb_load_provides (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT provide" &
        "  FROM pkg_provides, provides AS s" &
        "  WHERE package_id = ?1" &
        "    AND provide_id = s.id" &
        "  ORDER by provide DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_PROVIDES, sql);
   end pkgdb_load_provides;


   --------------------------------------------------------------------
   --  pkgdb_load_requires
   --------------------------------------------------------------------
   function pkgdb_load_requires (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                 return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT require" &
        "  FROM pkg_requires, requires AS s" &
        "  WHERE package_id = ?1" &
        "    AND require_id = s.id" &
        "  ORDER by require DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_REQUIRES, sql);
   end pkgdb_load_requires;


   --------------------------------------------------------------------
   --  pkgdb_load_conflicts
   --------------------------------------------------------------------
   function pkgdb_load_conflicts (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                  return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT packages.name" &
        "  FROM pkg_conflicts" &
        "    LEFT JOIN packages ON" &
        "    (packages.id = pkg_conflicts.conflict_id)" &
        "  WHERE package_id = ?1";

   begin
      return load_val (db, pkg_access, PKG_LOAD_CONFLICTS, sql);
   end pkgdb_load_conflicts;


   --------------------------------------------------------------------
   --  pkgdb_load_annotations
   --------------------------------------------------------------------
   function pkgdb_load_annotations (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                    return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT k.annotation AS tag, v.annotation AS value" &
        "  FROM pkg_annotation p" &
        "    JOIN annotation k ON (p.tag_id = k.annotation_id)" &
        "    JOIN annotation v ON (p.value_id = v.annotation_id)" &
        "  WHERE p.package_id = ?1" &
        "  ORDER BY tag, value";

   begin
      return load_val (db, pkg_access, PKG_LOAD_ANNOTATIONS, sql);
   end pkgdb_load_annotations;


   --------------------------------------------------------------------
   --  pkgdb_load_dirs
   --------------------------------------------------------------------
   function pkgdb_load_dirs (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                             return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT path, try" &
        "  FROM pkg_directories, directories" &
        "  WHERE package_id = ?1" &
        "    AND directory_id = directories.id" &
        "  ORDER by path DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_DIRS, sql);
   end pkgdb_load_dirs;


   --------------------------------------------------------------------
   --  pkgdb_load_scripts
   --------------------------------------------------------------------
   function pkgdb_load_scripts (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT script, type" &
        "  FROM pkg_script" &
        "    JOIN script USING(script_id)" &
        "  WHERE package_id = ?1";

   begin
      return load_val (db, pkg_access, PKG_LOAD_SCRIPTS, sql);
   end pkgdb_load_scripts;


   --------------------------------------------------------------------
   --  pkgdb_load_rdeps
   --------------------------------------------------------------------
   function pkgdb_load_rdeps (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                              return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT p.name, p.origin, p.version" &
        "  FROM packages AS p" &
        "    INNER JOIN deps AS d ON (p.id = d.package_id)" &
        "  WHERE d.name = ?1";

   begin
      return load_val (db, pkg_access, PKG_LOAD_RDEPS, sql);
   end pkgdb_load_rdeps;


   --------------------------------------------------------------------
   --  pkgdb_load_shlib_reqd
   --------------------------------------------------------------------
   function pkgdb_load_shlib_reqd (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                   return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT name" &
        "  FROM pkg_shlibs_required, shlibs AS s" &
        "  WHERE package_id = ?1" &
        "    AND shlib_id = s.id" &
        "  ORDER by name DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_SHLIBS_REQUIRED, sql);
   end pkgdb_load_shlib_reqd;


   --------------------------------------------------------------------
   --  pkgdb_load_shlib_prov
   --------------------------------------------------------------------
   function pkgdb_load_shlib_prov (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                   return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT name" &
        "  FROM pkg_shlibs_provided, shlibs AS s" &
        "  WHERE package_id = ?1" &
        "    AND shlib_id = s.id" &
        "  ORDER by name DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_SHLIBS_PROVIDED, sql);
   end pkgdb_load_shlib_prov;


   --------------------------------------------------------------------
   --  pkgdb_load_files
   --------------------------------------------------------------------
   function pkgdb_load_files (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                              return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT path, sha256" &
        "  FROM files" &
        "  WHERE package_id = ?1" &
        "  ORDER BY PATH ASC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_FILES, sql);
   end pkgdb_load_files;


   --------------------------------------------------------------------
   --  pkgdb_load_config_file
   --------------------------------------------------------------------
   function pkgdb_load_config_file (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                    return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT path, content" &
        "  FROM config_files" &
        "  WHERE package_id = ?1" &
        "  ORDER BY PATH ASC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_CONFIG_FILES, sql);
   end pkgdb_load_config_file;


   --------------------------------------------------------------------
   --  pkgdb_load_options
   --------------------------------------------------------------------
   function pkgdb_load_options (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT option, value" &
        "  FROM option" &
        "    JOIN pkg_option USING(option_id)" &
        "  WHERE package_id = ?1" &
        "  ORDER BY option";

   begin
      return load_val (db, pkg_access, PKG_LOAD_OPTIONS, sql);
   end pkgdb_load_options;


   --------------------------------------------------------------------
   --  pkgdb_load_deps
   --------------------------------------------------------------------
   function pkgdb_load_deps (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                             return Pkg_Error_Type
   is
      sql : constant String :=

        "SELECT d.name, d.origin, d.version" &
        "  FROM deps AS d" &
        "    LEFT JOIN packages AS p ON" &
        "    (p.origin = d.origin AND p.name = d.name)" &
        "  WHERE d.package_id = ?1" &
        "  ORDER BY d.origin DESC";

   begin
      return load_val (db, pkg_access, PKG_LOAD_DEPS, sql);
   end pkgdb_load_deps;


   --------------------------------------------------------------------
   --  pkgdb_load_dep_formula
   --------------------------------------------------------------------
   function pkgdb_load_dep_formula (db : sqlite_h.sqlite3_Access; pkg_access : T_pkg_Access)
                                    return Pkg_Error_Type
   is
      options_sql : constant String :=

        "SELECT option, value" &
        "  FROM option" &
        "    JOIN pkg_option USING(option_id)" &
        "  WHERE package_id = ?1" &
        "  ORDER BY option";

      formula_preamble : constant String :=

        "SELECT id, name, origin, version, locked FROM packages WHERE ";

   begin
      if (pkg_access.flags and load_on_flag (PKG_LOAD_DEP_FORMULA).flag) > 0 then
         return EPKG_OK;
      end if;

      if IsBlank (pkg_access.dep_formula) then
         pkg_access.flags := pkg_access.flags or load_on_flag (PKG_LOAD_DEP_FORMULA).flag;
         return EPKG_OK;
      end if;

      Event.pkg_debug
        (4, "Pkgdb: reading package formula '" & USS (pkg_access.dep_formula) & "'");

      declare
         procedure scan_formula (position : Deps.formula_crate.Cursor);
         procedure scan_item    (item_pos : Deps.pkg_dep_formula.Cursor);

         formula : Deps.formula_crate.Vector :=
           Deps.pkg_deps_parse_formula (USS (pkg_access.dep_formula));

         abort_scan        : Boolean := False;
         last_formula_item : Deps.pkg_dep_formula.Cursor;

         procedure scan_formula (position : Deps.formula_crate.Cursor)
         is
            one_formula : Deps.pkg_formula renames Deps.formula_crate.Element (position);
         begin
            if not abort_scan then
               last_formula_item := one_formula.items.Last;
               one_formula.items.Iterate (scan_item'Access);
            end if;
         end scan_formula;

         procedure scan_item (item_pos : Deps.pkg_dep_formula.Cursor)
         is
            use type Deps.pkg_dep_formula.Cursor;

            last_one : Boolean := (last_formula_item = item_pos);
            item     : Deps.pkg_dep_formula_item renames Deps.pkg_dep_formula.Element (item_pos);
            clause   : String := Deps.pkg_deps_formula_tosql (item, last_one);
            stmt     : aliased sqlite_h.sqlite3_stmt_Access;
            formula_sql : String := formula_preamble & clause;
         begin
            if abort_scan then
               return;
            end if;

            if clause'Length > 0 then
               Event.pkg_debug (4, "Pkgdb: running '" & formula_sql & "'");
               if not SQLite.prepare_sql (db, formula_sql, stmt'Access) then
                  PkgDB.ERROR_SQLITE (db, "pkgdb_load_dep_formula", formula_sql);
                  abort_scan := True;
               end if;

               --  Fetch matching packages
               declare
                  options_match : Boolean := True;
               begin
                  loop
                     exit when not SQLite.step_through_statement (stmt);
                     --  Load options for a package and check  if they are compatible
                     if not item.options.Is_Empty then
                        declare
                           procedure scan_opt (option_pos : Deps.pkg_dep_option_item_crate.Cursor);

                           opt_stmt : aliased sqlite_h.sqlite3_stmt_Access;

                           procedure scan_opt (option_pos : Deps.pkg_dep_option_item_crate.Cursor)
                           is
                              option : Deps.pkg_dep_option_item renames
                                Deps.pkg_dep_option_item_crate.Element (option_pos);
                              optname : String := SQLite.retrieve_string (opt_stmt, 0);
                              optval  : String := SQLite.retrieve_string (opt_stmt, 1);
                           begin
                              if options_match then
                                 if equivalent (option.option, optname) then
                                    if (option.active and then optval /= "on") or else
                                      (not option.active and then optval /= "off")
                                    then
                                       Event.pkg_debug (4, "incompatible option for " & optname &
                                                          ":" & optval);
                                       options_match := False;
                                    end if;
                                 end if;
                              end if;
                           end scan_opt;

                        begin
                           Event.pkg_debug (4, "Pkgdb: running '" & options_sql & "'");
                           if not SQLite.prepare_sql (db, options_sql, opt_stmt'Access) then
                              PkgDB.ERROR_SQLITE (db, "pkgdb_load_dep_formula", options_sql);
                              abort_scan := True;
                              exit;
                           end if;

                           SQLite.bind_integer (opt_stmt, 1, SQLite.sql_int64 (0));
                           loop
                              exit when not SQLite.step_through_statement (opt_stmt);
                              item.options.Iterate (scan_opt'Access);
                           end loop;
                        end;
                     end if;

                     if options_match then
                        declare
                           chain_result  : Pkg_Error_Type;
                           name    : Text := SUS (SQLite.retrieve_string (stmt, 1));
                           origin  : Text := SUS (SQLite.retrieve_string (stmt, 2));
                           version : Text := SUS (SQLite.retrieve_string (stmt, 3));
                           locked  : Boolean := SQLite.retrieve_boolean (stmt, 4);
                        begin
                           chain_result := pkg_adddep (pkg_access => pkg_access,
                                                       name       => name,
                                                       origin     => origin,
                                                       version    => version,
                                                       locked     => locked);
                        end;
                     end if;
                  end loop;
                  SQLite.finalize_statement (stmt);
               end;
            end if;
         end scan_item;

      begin
         formula.Iterate (scan_formula'Access);
         if abort_scan then
            return EPKG_FATAL;
         end if;
      end;

      pkg_access.flags := pkg_access.flags or load_on_flag (PKG_LOAD_DEP_FORMULA).flag;
      return EPKG_OK;
   end pkgdb_load_dep_formula;

end Core.Iterators.Binary_sqlite;
