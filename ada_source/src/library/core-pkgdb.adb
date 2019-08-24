--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Conversions;
with System;

with Core.Config;
with Core.Deps;

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
      if numargs /= 0 then
         errmsg := ICS.New_String ("Invalid usage of vercmp(): needs 3 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
      end if;


      declare
         op_str : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         arg1   : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (2));
         arg2   : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (3));
         op     : Deps.pkg_dep_version_op;
         cmp    : Integer;
         ret    : Boolean;
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
         --  cmp

         case op is
            when deps.VERSION_EQ  => ret := (cmp = 0);
            when Deps.VERSION_GE  => ret := (cmp >= 0);
            when Deps.VERSION_LE  => ret := (cmp <= 0);
            when Deps.VERSION_GT  => ret := (cmp > 0);
            when Deps.VERSION_LT  => ret := (cmp < 0);
            when Deps.VERSION_NOT => ret := (cmp != 0);
            when Deps.VERSION_ANY => ret := True;
         end;

         if ret then
            sqlite_h.sqlite3_result_int (context, IC.int (1));
         else
            sqlite_h.sqlite3_result_int (context, IC.int (0));
         end if;
   end pkgdb_vercmp;


   --------------------------------------------------------------------
   --  pkgdb_regex
   --------------------------------------------------------------------
   procedure pkgdb_regex
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
   begin
      null;
   end pkgdb_regex;


   --------------------------------------------------------------------
   --  pkgdb_regex
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

end Core.PkgDB;
