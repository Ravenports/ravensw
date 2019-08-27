--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Conversions;
with System;

with Core.Config;
with Core.Deps;
with Core.Version;
with Regex;
with SQLite;

package body Core.PkgDB is

   --------------------------------------------------------------------
   --  pkgshell_open
   --------------------------------------------------------------------
   procedure pkgshell_open (reponame : access ICS.chars_ptr)
   is
      dbdir  : constant String := Config.pkg_config_get_string (Config.conf_dbdir);
      dbfile : constant String := dbdir & "/local.sqlite";
      result : IC.int;
   begin
      reponame.all := ICS.New_String (dbfile);
      result := sqlite_h.sqlite3_auto_extension (callback => pkgdb_sqlcmd_init'Access);
   end pkgshell_open;


   --------------------------------------------------------------------
   --  pkgdb_sqlcmd_init
   --------------------------------------------------------------------
   function pkgdb_sqlcmd_init
     (db       : not null sqlite_h.sqlite3_Access;
      pzErrMsg : not null access ICS.chars_ptr;
      pThunk   : not null sqlite_h.sqlite3_api_routines_Access) return IC.int
   is
      procedure fast (name : String; nargs : Natural; cb : sqlite_h.cb_xFuncStep);
      procedure fast (name : String; nargs : Natural; cb : sqlite_h.cb_xFuncStep)
      is
         use type IC.int;
         flags : constant IC.int := sqlite_h.SQLITE_ANY + sqlite_h.SQLITE_DETERMINISTIC;
         result : IC.int;
      begin
         result := sqlite_h.sqlite3_create_function
           (db            => db,
            zFunctionName => ICS.New_String (name),
            nArg          => IC.int (nargs),
            eTextRep      => flags,
            pApp          => System.Null_Address,
            xFunc         => cb,
            xStep         => null,
            xFinal        => null);
      end fast;
   begin
      fast ("now",    0, pkgdb_now'Access);
      fast ("myarch", 0, pkgdb_myarch'Access);
      fast ("myarch", 1, pkgdb_myarch'Access);
      fast ("regexp", 2, pkgdb_regex'Access);
      fast ("vercmp", 3, pkgdb_vercmp'Access);
      fast ("split_version", 2, pkgdb_split_version'Access);

      return sqlite_h.SQLITE_OK;
   end pkgdb_sqlcmd_init;


   --------------------------------------------------------------------
   --  pkgdb_vercmp
   --------------------------------------------------------------------
   procedure pkgdb_vercmp
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      argv   : array (1 .. 3) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;
   begin
      if numargs /= 3 then
         errmsg := ICS.New_String ("Invalid usage of vercmp(): needs 3 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
      end if;

      declare
         op_str : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         arg1   : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (2));
         arg2   : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (3));
         op     : Deps.pkg_dep_version_op;
         cmp    : Version.cmp_result;
         ret    : Boolean;
         result : IC.int := 0;
         use type ICS.chars_ptr;
      begin
         if op_str = ICS.Null_Ptr or else
           arg1 = ICS.Null_Ptr or else
           arg2 = ICS.Null_Ptr
         then
            errmsg := ICS.New_String ("Invalid usage of vercmp(): blank arguments");
            sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
            ICS.Free (errmsg);
         end if;

         op := Deps.pkg_deps_string_toop (ICS.Value (op_str));
         cmp := Version.pkg_version_cmp (ICS.Value (arg1), ICS.Value (arg2));

         case op is
            when Deps.VERSION_EQ  => ret := (cmp = 0);
            when Deps.VERSION_GE  => ret := (cmp >= 0);
            when Deps.VERSION_LE  => ret := (cmp <= 0);
            when Deps.VERSION_GT  => ret := (cmp > 0);
            when Deps.VERSION_LT  => ret := (cmp < 0);
            when Deps.VERSION_NOT => ret := (cmp /= 0);
            when Deps.VERSION_ANY => ret := True;
         end case;

         if ret then
            result := IC.int (1);
         end if;
         sqlite_h.sqlite3_result_int (context, result);
      end;
   end pkgdb_vercmp;


   --------------------------------------------------------------------
   --  pkgdb_regex
   --------------------------------------------------------------------
   procedure pkgdb_regex
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      argv   : array (1 .. 2) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;

   begin
      if numargs /= 2 then
         errmsg := ICS.New_String ("Invalid usage of regex(): needs 2 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
      end if;

      declare
         regex     : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         str       : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (2));
         re_Access : regex_h.regex_t_Access;
         ret       : IC.int;

         use type ICS.chars_ptr;
         use type regex_h.regex_t_Access;
      begin
         if regex = ICS.Null_Ptr or else
           str = ICS.Null_Ptr
         then
            errmsg := ICS.New_String ("Invalid usage of regex(): blank arguments");
            sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
            ICS.Free (errmsg);
         end if;

         re_Access := SQLite.sqlite3_get_auxdata_as_regex (context, 0);

         if re_Access = null then
            declare
               cflags : IC.int;
               res    : IC.int;
            begin
               if pkgdb_is_case_sensitive then
                  cflags := IC.int (regex_h.REG_EXTENDED + regex_h.REG_NOSUB);
               else
                  cflags := IC.int (regex_h.REG_EXTENDED + regex_h.REG_NOSUB + regex_h.REG_ICASE);
               end if;

               re_Access := re'Access;
               res := regex_h.regcomp (re_Access, regex, cflags);
               if (res /= 0) then
                  errmsg := ICS.New_String ("Invalid regex");
                  sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
                  ICS.Free (errmsg);
               end if;

               SQLite.sqlite3_set_auxdata_as_regex (context  => context,
                                                    N        => 0,
                                                    data     => re_Access,
                                                    callback => pkgdb_regex_delete'Access);
            end;
         end if;
         ret := regex_h.regexec (preg   => re_Access,
                                 regex  => str,
                                 nmatch => 0,
                                 pmatch => null,
                                 eflags => 0);
         sqlite_h.sqlite3_result_int (context => context,
                                      result  => conv2cint (ret /= regex_h.REG_NOMATCH));
      end;
   end pkgdb_regex;


   --------------------------------------------------------------------
   --  pkgdb_myarch
   --------------------------------------------------------------------
   procedure pkgdb_myarch
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
   begin
      null;
   end pkgdb_myarch;


   --------------------------------------------------------------------
   --  pkgdb_split_version
   --------------------------------------------------------------------
   procedure pkgdb_split_version
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
   begin
      null;
   end pkgdb_split_version;


   --------------------------------------------------------------------
   --  pkgdb_now
   --------------------------------------------------------------------
   procedure pkgdb_now
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      Now    : Ada.Calendar.Time := Ada.Calendar.Clock;
      epoch  : IC.long := Ada.Calendar.Conversions.To_Unix_Time (Now);
   begin
      if numargs /= 0 then
         errmsg := ICS.New_String ("Invalid usage of now(): no arguments expected");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
      end if;

      sqlite_h.sqlite3_result_int64 (context, sqlite_h.sql64 (epoch));
   end pkgdb_now;


   --------------------------------------------------------------------
   --  pkgdb_set_case_sensitivity
   --------------------------------------------------------------------
   procedure pkgdb_set_case_sensitivity (sensitive : Boolean) is
   begin
      case_sensitivity_setting := sensitive;
   end pkgdb_set_case_sensitivity;


   --------------------------------------------------------------------
   --  pkgdb_get_case_sensitivity
   --------------------------------------------------------------------
   function pkgdb_is_case_sensitive return Boolean is
   begin
      return case_sensitivity_setting;
   end pkgdb_is_case_sensitive;


   --------------------------------------------------------------------
   --  pkgdb_regex_delete
   --------------------------------------------------------------------
   procedure pkgdb_regex_delete (regex_ptr : not null regex_h.regex_t_Access) is
   begin
      regex_h.regfree (regex_ptr);
      Regex.free (regex_ptr);
   end pkgdb_regex_delete;


   --------------------------------------------------------------------
   --  conv2cint
   --------------------------------------------------------------------
   function conv2cint (result : Boolean) return IC.int is
   begin
      if result then
         return IC.int (1);
      else
         return IC.int (0);
      end if;
   end conv2cint;


end Core.PkgDB;
