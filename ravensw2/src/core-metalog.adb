--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Directories;

with Core.Strings;  use Core.Strings;
with Core.Event;

package body Core.Metalog is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EV  renames Core.Event;

   metalog_handle : TIO.File_Type;


   --------------------------------------------------------------------------------------------
   --  metalog_open
   --------------------------------------------------------------------------------------------
   function metalog_open (filename : String) return Action_Result is
   begin
      if DIR.Exists (filename) then
         TIO.Open (File => metalog_handle,
                   Mode => TIO.Append_File,
                   Name => filename);
      else
         TIO.Create (File => metalog_handle,
                     Mode => TIO.Out_File,
                     Name => filename);
      end if;
      return RESULT_OK;
   exception
      when others =>
         EV.emit_error ("Unable to open metalog '" & filename & "'");
         return RESULT_FATAL;
   end metalog_open;


   --------------------------------------------------------------------------------------------
   --  metalog_add
   --------------------------------------------------------------------------------------------
   procedure metalog_add (mtype  : pkg_metalog_type;
                          path   : String;
                          uname  : String;
                          gname  : String;
                          link   : String;
                          mode   : Integer;
                          fflags : Natural)
   is
   begin
      if not TIO.Is_Open (metalog_handle) then
         return;
      end if;

      declare
         mode_str : String := octal (mode, 3, True);
      begin
         case mtype is
            when PKG_METALOG_DIR =>
               TIO.Put_Line (metalog_handle, "./" & path &
                               " type=dir " &
                               " uname=" & uname &
                               " gname=" & gname &
                               " mode=" & mode_str & strtoflags_plus_label (fflags));
            when PKG_METALOG_FILE =>
               TIO.Put_Line (metalog_handle, "./" & path &
                               " type=file " &
                               " uname=" & uname &
                               " gname=" & gname &
                               " mode=" & mode_str & strtoflags_plus_label (fflags));
            when PKG_METALOG_LINK =>
               TIO.Put_Line (metalog_handle, "./" & path &
                               " type=link " &
                               " uname=" & uname &
                               " gname=" & gname &
                               " mode=" & mode_str &
                               " link=" & link & strtoflags_plus_label (fflags));
         end case;
      exception
         when others =>
            EV.emit_with_strerror ("Unable to write to the metalog");
      end;

   end metalog_add;


   --------------------------------------------------------------------------------------------
   --  metalog_close
   --------------------------------------------------------------------------------------------
   procedure metalog_close is
   begin
      if TIO.Is_Open (metalog_handle) then
         TIO.Close (metalog_handle);
      end if;
   exception
      when others => null;
   end metalog_close;


   --------------------------------------------------------------------------------------------
   --  strtofflags_supported
   --------------------------------------------------------------------------------------------
   function strtofflags_supported return Boolean is
   begin
      case platform is
         when freebsd | dragonfly | macos => return True;
         when netbsd | openbsd            => return True;
         when solaris | linux | omnios    => return False;
         when generic_unix                => return False;
      end case;
   end strtofflags_supported;


   --------------------------------------------------------------------------------------------
   --  strtoflags_plus_label
   --------------------------------------------------------------------------------------------
   function strtoflags_plus_label (fflags : Natural) return String
   is
      type T_flags is mod 2 ** 24;
      subtype places is Natural range 0 .. 23;

      function positive_flag (place : places) return String;

      xflags : T_flags := T_flags (fflags);
      result : Text;
      track  : Natural := 0;
      seen   : Boolean := False;

      function positive_flag (place : places) return String is
      begin
         case place is
            when  0 => return "nodump";
            when  1 => return "uimmutable";
            when  2 => return "uappend";
            when  3 => return "opaque";
            when  4 => return "nouunlink";
            when  5 => return "UF_FBSDRSVD20";   --  unused
            when  6 => return "nouhistory";
            when  7 => return "cache";
            when  8 => return "xlink";
            when  9 ..
                 15 => return "undefined";
            when 16 => return "archived";
            when 17 => return "simmutable";
            when 18 => return "sappend";
            when 19 => return "sunlink";
            when 20 => return "SF_FBSDRSVD20";   --  Internal use on FreeBSD
            when 21 => return "shistory";
            when 22 => return "noscache";
            when 23 => return "sxlink";
         end case;
      end positive_flag;
   begin
      if not strtofflags_supported or else fflags = 0 then
         return "";
      end if;

      result := SUS (" flags=");
      for x in places'Range loop
         case x is
            when 5 | 9 .. 15 | 20 => null;
            when others =>
               if seen then
                  SU.Append (result, ',');
               else
                  seen := True;
               end if;
               if (T_flags (2 ** x) and xflags) > 0 then
                  SU.Append (result, positive_flag (x));
               end if;
         end case;
      end loop;

      return USS (result);
   end strtoflags_plus_label;


end Core.Metalog;
