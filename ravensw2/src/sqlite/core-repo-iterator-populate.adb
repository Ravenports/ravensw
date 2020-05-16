--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Strings;
with Core.Event;
with Core.Utilities;
with Core.Context;
with SQLite;

use Core.Strings;

package body Core.Repo.Iterator.Populate is

   --------------------------------------------------------------------
   --  populate_pkg
   --------------------------------------------------------------------
   procedure populate_pkg (stmt : sqlite_h.sqlite3_stmt_Access;
                           pkg_access : Pkgtypes.A_Package_Access)
   is
      function get_text (col_index : Natural) return Text;
      function get_boolean (col_index : Natural) return Boolean;
      function get_pkgsize (col_index : Natural) return Pkgtypes.Package_Size;
      function get_pkgid (col_index : Natural) return Pkgtypes.Package_ID;
      function get_timestamp (col_index : Natural) return Pkgtypes.Package_Timestamp;
      function get_message (col_index : Natural) return Pkgtypes.Package_Message;
      function get_liclogic (col_index : Natural) return Pkgtypes.License_Logic;

      P : Pkgtypes.A_Package_Access renames pkg_access;

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

      function get_pkgsize (col_index : Natural) return Pkgtypes.Package_Size is
      begin
         return Pkgtypes.Package_Size (SQLite.retrieve_integer (stmt, col_index));
      end get_pkgsize;

      function get_pkgid (col_index : Natural) return Pkgtypes.Package_ID is
      begin
         return Pkgtypes.Package_ID (SQLite.retrieve_integer (stmt, col_index));
      end get_pkgid;

      function get_timestamp (col_index : Natural) return Pkgtypes.Package_Timestamp is
      begin
         return Pkgtypes.Package_Timestamp (SQLite.retrieve_integer (stmt, col_index));
      end get_timestamp;

      function get_message (col_index : Natural) return Pkgtypes.Package_Message
      is
         result : Pkgtypes.Package_Message;
      begin
         result.contents := get_text (col_index);
         return result;
      end get_message;

      function get_liclogic (col_index : Natural) return Pkgtypes.License_Logic
      is
         r64  : SQLite.sql_int64 := SQLite.retrieve_integer (stmt, col_index);
         AOR  : constant SQLite.sql_int64 := SQLite.sql_int64 (Character'Pos ('|'));
         AAND : constant SQLite.sql_int64 := SQLite.sql_int64 (Character'Pos ('&'));
      begin
         case r64 is
            when AOR    => return Pkgtypes.LICENSE_OR;
            when AAND   => return Pkgtypes.LICENSE_AND;
            when 1      => return Pkgtypes.LICENSE_SINGLE;
            when others =>
               Event.emit_notice
                 ("Invalid license logic value " & int2str (Integer (r64)) &
                    ", defaulting to single license");
               return Pkgtypes.LICENSE_SINGLE;
         end case;
      end get_liclogic;

   begin
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
                  Event.emit_error ("populate_pkg(): unknown column " & colname);
            end case;
         end;
      end loop;
   end populate_pkg;


   --------------------------------------------------------------------
   --  get_attribute
   --------------------------------------------------------------------
   function get_attribute (column_name : String) return pkg_attr
   is
      num_aliases : constant := 1;  -- id/rowid
      num_attr    : constant := pkg_attr'Pos (pkg_attr'Last) + 1;
      total_keywords : constant Positive := num_attr + num_aliases;

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
         ("abi           ", PKG_ABI),
         ("annotations   ", PKG_ANNOTATIONS),
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
         ("mtree         ", PKG_MTREE),
         ("name          ", PKG_NAME),
         ("numfields     ", PKG_NUM_FIELDS),
         ("olddigest     ", PKG_OLD_DIGEST),
         ("oldflatsize   ", PKG_OLD_FLATSIZE),
         ("oldversion    ", PKG_OLD_VERSION),
         ("origin        ", PKG_ORIGIN),
         ("pkgsize       ", PKG_PKGSIZE),
         ("prefix        ", PKG_PREFIX),
         ("reason        ", PKG_REASON),
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
   --  add_category
   --------------------------------------------------------------------
   function add_category
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.categories.Append (data);
      return RESULT_OK;
   end add_category;


   --------------------------------------------------------------------
   --  add_license
   --------------------------------------------------------------------
   function add_license
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.licenses.Append (data);
      return RESULT_OK;
   end add_license;


   --------------------------------------------------------------------
   --  add_user
   --------------------------------------------------------------------
   function add_user
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.users.Append (data);
      return RESULT_OK;
   end add_user;


   --------------------------------------------------------------------
   --  add_group
   --------------------------------------------------------------------
   function add_group
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.groups.Append (data);
      return RESULT_OK;
   end add_group;


   --------------------------------------------------------------------
   --  add_shlib_prov
   --------------------------------------------------------------------
   function add_shlib_prov
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.shlibs_prov.Append (data);
      return RESULT_OK;
   end add_shlib_prov;


   --------------------------------------------------------------------
   --  add_shlib_reqd
   --------------------------------------------------------------------
   function add_shlib_reqd
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.shlibs_reqd.Append (data);
      return RESULT_OK;
   end add_shlib_reqd;


   --------------------------------------------------------------------
   --  add_provides
   --------------------------------------------------------------------
   function add_provides
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.provides.Append (data);
      return RESULT_OK;
   end add_provides;


   --------------------------------------------------------------------
   --  add_requires
   --------------------------------------------------------------------
   function add_requires
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      data : Text := SUS (SQLite.retrieve_string (stmt, 0));
   begin
      pkg_access.requires.Append (data);
      return RESULT_OK;
   end add_requires;


   --------------------------------------------------------------------
   --  add_conflict
   --------------------------------------------------------------------
   function add_conflict
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      unique_id : Text := SUS (SQLite.retrieve_string (stmt, 0));
      conflict : Pkgtypes.Package_Conflict;
   begin
      --  silently ignore duplicates in case of conflicts
      if not pkg_access.conflicts.Contains (unique_id) then
         Event.emit_debug (3, "Pkg: add a new conflict origin: " & USS (pkg_access.uid) &
                            " with " & USS (unique_id));
         --  don't set conflict.digest or conflict.contype right now
         conflict.uid := unique_id;
         pkg_access.conflicts.Insert (unique_id, conflict);
      end if;
      return RESULT_OK;
   end add_conflict;


   --------------------------------------------------------------------
   --  add_annotation
   --------------------------------------------------------------------
   function add_annotation
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      name  : Text := SUS (SQLite.retrieve_string (stmt, 0));
      value : Text := SUS (SQLite.retrieve_string (stmt, 1));
   begin
      if not pkg_access.annotations.Contains (name) then
         pkg_access.annotations.Insert (name, value);
      end if;
      return RESULT_OK;
   end add_annotation;


   --------------------------------------------------------------------
   --  add_directory
   --------------------------------------------------------------------
   function add_directory
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
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
   function add_scripts
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      use type SQLite.sql_int64;

      script : Text := SUS (SQLite.retrieve_string (stmt, 0));
      type_index : SQLite.sql_int64 := SQLite.retrieve_integer (stmt, 1);
   begin
      if type_index < 0 or else
        type_index > SQLite.sql_int64 (Pkgtypes.Package_Script'Pos (Pkgtypes.Package_Script'Last))
      then
         Event.emit_error ("add_scripts(): script type out of range");
         return RESULT_FATAL;
      end if;

      pkg_access.scripts (Pkgtypes.Package_Script'Val (type_index)) := script;
      return RESULT_OK;
   end add_scripts;


   --------------------------------------------------------------------
   --  add_rdeps
   --------------------------------------------------------------------
   function add_rdeps
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
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
   --  add_files
   --------------------------------------------------------------------
   function add_files
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
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
   --  add_config_files
   --------------------------------------------------------------------
   function add_config_files
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      path    : Text := SUS (SQLite.retrieve_string (stmt, 0));
      content : Text := SUS (SQLite.retrieve_string (stmt, 1));
   begin
      return pkg_addconfig_file (pkg_access, path, content);
   end add_config_files;


   --------------------------------------------------------------------
   --  add_options
   --------------------------------------------------------------------
   function add_options
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
   is
      name  : Text := SUS (SQLite.retrieve_string (stmt, 0));
      value : Text := SUS (SQLite.retrieve_string (stmt, 1));
   begin
      return pkg_addoption (pkg_access, name, value);
   end add_options;


   --------------------------------------------------------------------
   --  add_deps
   --------------------------------------------------------------------
   function add_deps
     (stmt : sqlite_h.sqlite3_stmt_Access;
      pkg_access : Pkgtypes.A_Package_Access) return Action_Result
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
   --  pkg_adddir_attr
   --------------------------------------------------------------------
   function pkg_adddir_attr
     (pkg_access : Pkgtypes.A_Package_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : Pkgtypes.mode_t;
      fflags : Pkgtypes.Package_Dir_Flags;
      check_duplicates : Boolean) return Action_Result
   is
      D : Pkgtypes.Package_Directory;
      fullpath : Text;
   begin
      if not contains (path, "/") then
         Event.emit_error ("skipping useless directory: " & SQ (USS (path)));
      end if;

      fullpath := SUS (Core.Utilities.pkg_absolutepath (USS (path), False));
      Event.emit_debug (3, "pop: add new directory " & SQ (USS (fullpath)));
      if check_duplicates and then
        pkg_access.dirs.Contains (fullpath)
      then
         declare
            prerror : constant String := "duplicate directory listing: " & USS (fullpath) & ", ";
         begin
            if Context.reveal_developer_mode then
               Event.emit_error (prerror & "fatal (developer mode)");
               return RESULT_FATAL;
            else
               Event.emit_error (prerror & "ignoring");
               return RESULT_OK;
            end if;
         end;
      end if;

      D.path   := fullpath;
      D.uname  := uname;
      D.gname  := gname;
      D.perm   := perm;
      D.fflags := fflags;

      pkg_access.dirs.Insert (fullpath, D);
      return RESULT_OK;
   end pkg_adddir_attr;


   --------------------------------------------------------------------
   --  pkg_addfile_attr
   --------------------------------------------------------------------
   function pkg_addfile_attr
     (pkg_access : Pkgtypes.A_Package_Access;
      path   : Text;
      uname  : Text;
      gname  : Text;
      perm   : Pkgtypes.mode_t;
      fflags : Pkgtypes.Package_Dir_Flags;
      sum    : Text;
      check_duplicates : Boolean) return Action_Result
   is
      F : Pkgtypes.Package_File;
      fullpath : Text;
   begin
      fullpath := SUS (Core.Utilities.pkg_absolutepath (USS (path), False));
      Event.emit_debug (3, "pop: add new file " & SQ (USS (fullpath)));
      if check_duplicates and then
        pkg_access.dirs.Contains (fullpath)
      then
         declare
            prerror : constant String := "duplicate file listing: " & USS (fullpath) & ", ";
         begin
            if Context.reveal_developer_mode then
               Event.emit_error (prerror & "fatal (developer mode)");
               return RESULT_FATAL;
            else
               Event.emit_error (prerror & "ignoring");
               return RESULT_OK;
            end if;
         end;
      end if;

      F.path   := fullpath;
      F.uname  := uname;
      F.gname  := gname;
      F.perm   := perm;
      F.fflags := fflags;
      F.sum    := sum;

      pkg_access.files.Append (F);
      return RESULT_OK;
   end pkg_addfile_attr;


   --------------------------------------------------------------------
   --  pkg_adddep
   --------------------------------------------------------------------
   function pkg_adddep
     (pkg_access : Pkgtypes.A_Package_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Action_Result
   is
      D : Pkgtypes.Package_Dependency;
   begin
      Event.emit_debug
        (3, "pop: add new dependency origin: " & USS (origin) & ", name: " & USS (name));

      if pkg_access.depends.Contains (name) then
         Event.emit_error
           (USS (pkg_access.name) & ": duplicate dependency listing: " & USS (name));
         return RESULT_FATAL;
      end if;

      D.name    := name;
      D.origin  := origin;
      D.version := version;
      D.locked  := locked;
      D.uid     := name;

      pkg_access.depends.Insert (name, D);
      return RESULT_OK;
   end pkg_adddep;


   --------------------------------------------------------------------
   --  pkg_addrdep
   --------------------------------------------------------------------
   function pkg_addrdep
     (pkg_access : Pkgtypes.A_Package_Access;
      name       : Text;
      origin     : Text;
      version    : Text;
      locked     : Boolean) return Action_Result
   is
      D : Pkgtypes.Package_Dependency;
   begin
      Event.emit_debug
        (3, "pop: add new reverse dependency origin: " & USS (origin) & ", name: " & USS (name));

      D.name    := name;
      D.origin  := origin;
      D.version := version;
      D.locked  := locked;
      D.uid     := name;

      pkg_access.rdepends.Prepend (D);
      return RESULT_OK;
   end pkg_addrdep;


   --------------------------------------------------------------------
   --  pkg_addoption
   --------------------------------------------------------------------
   function pkg_addoption
     (pkg_access : Pkgtypes.A_Package_Access;
      key        : Text;
      value      : Text) return Action_Result
   is
   begin
      Event.emit_debug (2, "pop> adding options: " & USS (key) & " = " & USS (value));
      if pkg_access.options.Contains (key) then
         declare
            prerror : constant String := "duplicate option listing: " & USS (key) & ", ";
         begin
            if Context.reveal_developer_mode then
               Event.emit_error (prerror & "fatal (developer mode)");
               return RESULT_FATAL;
            else
               Event.emit_error (prerror & "ignoring");
               return RESULT_OK;
            end if;
         end;
      end if;

      pkg_access.options.Insert (key, value);
      return RESULT_OK;
   end pkg_addoption;


   --------------------------------------------------------------------
   --  pkg_addconfig_file
   --------------------------------------------------------------------
   function pkg_addconfig_file
     (pkg_access : Pkgtypes.A_Package_Access;
      path       : Text;
      content    : Text) return Action_Result
   is
      CF : Pkgtypes.Package_Config_File;
      fullpath : Text;
   begin
      fullpath := SUS (Core.Utilities.pkg_absolutepath (USS (path), False));
      Event.emit_debug (3, "pop: add new config file '" & USS (fullpath) & "'");
      if pkg_access.config_files.Contains (fullpath)
      then
         declare
            prerror : constant String := "duplicate config file listing: " & USS (fullpath) & ", ";
         begin
            if Context.reveal_developer_mode then
               Event.emit_error (prerror & "fatal (developer mode)");
               return RESULT_FATAL;
            else
               Event.emit_error (prerror & "ignoring");
               return RESULT_OK;
            end if;
         end;
      end if;

      CF.path := fullpath;
      CF.content := content;

      pkg_access.config_files.Insert (fullpath, CF);
      return RESULT_OK;
   end pkg_addconfig_file;


end Core.Repo.Iterator.Populate;
