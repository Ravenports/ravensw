--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Calendar.Conversions;
with System;

with Core.Strings;
with Core.Config;
with Core.Version;
with Core.Depends;
with SQLite;

use Core.Strings;

package body Core.Database.CustomCmds is

   --------------------------------------------------------------------
   --  rdb_vercmp
   --------------------------------------------------------------------
   procedure rdb_vercmp
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      argv   : array (1 .. 3) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if numargs /= 3 then
         errmsg := ICS.New_String ("Invalid usage of vercmp(): needs 3 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      declare
         op_str : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         arg1   : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (2));
         arg2   : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (3));
         op     : Depends.Dep_Version_Operator;
         cmp    : Version.Cmp_Result;
         ret    : Boolean;
         result : IC.int := 0;
         use type ICS.chars_ptr;
      begin
         if op_str = ICS.Null_Ptr or else
           arg1 = ICS.Null_Ptr or else
           arg2 = ICS.Null_Ptr
         then
            errmsg := ICS.New_String ("Invalid usage of vercmp(): null arguments");
            sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
            ICS.Free (errmsg);
            return;
         end if;

         op := Depends.string_to_operator (ICS.Value (op_str));
         cmp := Version.pkg_version_cmp (ICS.Value (arg1), ICS.Value (arg2));

         case op is
            when Depends.VERSION_EQ  => ret := (cmp = 0);
            when Depends.VERSION_GE  => ret := (cmp >= 0);
            when Depends.VERSION_LE  => ret := (cmp <= 0);
            when Depends.VERSION_GT  => ret := (cmp > 0);
            when Depends.VERSION_LT  => ret := (cmp < 0);
            when Depends.VERSION_NOT => ret := (cmp /= 0);
            when Depends.VERSION_ANY => ret := True;
         end case;

         if ret then
            result := IC.int (1);
         end if;
         sqlite_h.sqlite3_result_int (context, result);
      end;
   end rdb_vercmp;


   --------------------------------------------------------------------
   --  rdb_regex
   --------------------------------------------------------------------
   procedure rdb_regex
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      argv   : array (1 .. 2) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if numargs /= 2 then
         errmsg := ICS.New_String ("Invalid usage of regex(): needs 2 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
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
            errmsg := ICS.New_String ("Invalid usage of regex(): null arguments");
            sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
            ICS.Free (errmsg);
            return;
         end if;

         re_Access := SQLite.sqlite3_get_auxdata_as_regex (context, 0);

         if re_Access = null then
            declare
               cflags : IC.int;
               res    : IC.int;
            begin
               if case_sensitivity_is_on then
                  cflags := IC.int (regex_h.REG_EXTENDED + regex_h.REG_NOSUB);
               else
                  cflags := IC.int (regex_h.REG_EXTENDED + regex_h.REG_NOSUB + regex_h.REG_ICASE);
               end if;

               re_Access := re'Access;
               res := regex_h.regcomp (re_Access, regex, cflags);
               if res /= 0 then
                  errmsg := ICS.New_String ("Invalid regex");
                  sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
                  ICS.Free (errmsg);
                  return;
               end if;

               SQLite.sqlite3_set_auxdata_as_regex (context  => context,
                                                    N        => 0,
                                                    data     => re_Access,
                                                    callback => rdb_regex_delete'Access);
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
   end rdb_regex;


   --------------------------------------------------------------------
   --  rdb_myarch
   --------------------------------------------------------------------
   procedure rdb_myarch
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access)
   is
      use type ICS.chars_ptr;
      use type IC.int;

      errmsg   : ICS.chars_ptr;
      first    : ICS.chars_ptr := ICS.Null_Ptr;
      show_abi : Boolean := False;
   begin
      if numargs > 1 then
         errmsg := ICS.New_String ("Invalid usage of myarch(): needs 0 or 1 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      if numargs = 0 then
         show_abi := True;
      else
         first := sqlite_h.sqlite3_value_text (argsval.all);
         if first = ICS.Null_Ptr then
            show_abi := True;
         end if;
      end if;

      if show_abi then
         declare
            abi : constant String := Config.configuration_value (Config.abi);
         begin
            first := ICS.New_String (abi);
            sqlite_h.sqlite3_result_text (context    => context,
                                          result     => first,
                                          termpos    => -1,
                                          destructor => null);
            ICS.Free (first);
         end;
      else
         sqlite_h.sqlite3_result_text (context    => context,
                                       result     => first,
                                       termpos    => -1,
                                       destructor => null);
      end if;

   end rdb_myarch;


   --------------------------------------------------------------------
   --  rdb_split_version
   --------------------------------------------------------------------
   procedure rdb_split_version
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access) is
   begin
      rdb_split_common (context => context,
                          numargs => numargs,
                          argsval => argsval,
                          delim   => '-',
                          first   => "name",
                          second  => "version");
   end rdb_split_version;


   --------------------------------------------------------------------
   --  rdb_now
   --------------------------------------------------------------------
   procedure rdb_now
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
         return;
      end if;

      sqlite_h.sqlite3_result_int64 (context, sqlite_h.sql64 (epoch));
   end rdb_now;


   --------------------------------------------------------------------
   --  rdb_split_common
   --------------------------------------------------------------------
   procedure rdb_split_common
     (context : not null sqlite_h.sqlite3_context_Access;
      numargs : IC.int;
      argsval : not null access sqlite_h.sqlite3_value_Access;
      delim   : Character;
      first   : String;
      second  : String)
   is
      use type IC.int;

      errmsg : ICS.chars_ptr;
      argv   : array (1 .. 2) of sqlite_h.sqlite3_value_Access;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if numargs /= 2 then
         errmsg := ICS.New_String ("Invalid usage of split_*(): needs 2 arguments");
         sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
         ICS.Free (errmsg);
         return;
      end if;

      declare
         what : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (1));
         data : ICS.chars_ptr := sqlite_h.sqlite3_value_text (argv (2));

         use type ICS.chars_ptr;
      begin
         if what = ICS.Null_Ptr or else
           data = ICS.Null_Ptr
         then
            errmsg := ICS.New_String ("Invalid usage of split_*(): null arguments");
            sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
            ICS.Free (errmsg);
            return;
         end if;

         declare
            whatstr  : constant String := ICS.Value (what);
            datastr  : constant String := ICS.Value (data);
            delimstr : constant String (1 .. 1) := (1 => delim);
         begin
            if whatstr = first then
               if contains (datastr, delimstr) then
                  sqlite_h.sqlite3_result_text
                    (context    => context,
                     result     => data,
                     termpos    => IC.int (head (datastr, delimstr)'Length),
                     destructor => null);
               else
                  sqlite_h.sqlite3_result_text (context    => context,
                                                result     => data,
                                                termpos    => IC.int (-1),
                                                destructor => null);
               end if;
            elsif whatstr = second then
               if contains (datastr, delimstr) then
                  declare
                     tailstring : constant String := tail (datastr, delimstr);
                     newstr     : ICS.chars_ptr;
                  begin
                     newstr := ICS.New_String (tailstring);
                     sqlite_h.sqlite3_result_text (context    => context,
                                                   result     => newstr,
                                                   termpos    => IC.int (-1),
                                                   destructor => null);
                     ICS.Free (newstr);
                  end;
               else
                  sqlite_h.sqlite3_result_text (context    => context,
                                                result     => data,
                                                termpos    => IC.int (-1),
                                                destructor => null);
               end if;
            else
               errmsg := ICS.New_String ("SQL function split_*() called with invalid arguments");
               sqlite_h.sqlite3_result_error (context, errmsg, IC.int (-1));
               ICS.Free (errmsg);
               return;
            end if;
         end;
      end;
   end rdb_split_common;


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


   --------------------------------------------------------------------
   --  rdb_regex_delete
   --------------------------------------------------------------------
   procedure rdb_regex_delete (regex_ptr : not null regex_h.regex_t_Access) is
   begin
      regex_h.regfree (regex_ptr);
   end rdb_regex_delete;


   --------------------------------------------------------------------
   --  sqlcmd_init
   --------------------------------------------------------------------
   function sqlcmd_init
     (db       : not null sqlite_h.sqlite3_Access;
      pzErrMsg : access ICS.chars_ptr;
      pThunk   : sqlite_h.sqlite3_api_routines_Access) return IC.int
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
      fast ("now",    0, rdb_now'Access);
      fast ("myarch", 0, rdb_myarch'Access);
      fast ("myarch", 1, rdb_myarch'Access);
      fast ("regexp", 2, rdb_regex'Access);
      fast ("vercmp", 3, rdb_vercmp'Access);
      fast ("split_version", 2, rdb_split_version'Access);

      return sqlite_h.SQLITE_OK;
   end sqlcmd_init;

end Core.Database.CustomCmds;
