--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Directories;
with Core.Event;
with Core.Strings;  use Core.Strings;

package body Core.Metalog is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EV  renames Core.Event;

   metalog_handle : TIO.File_Type;


   --------------------------------------------------------------------------------------------
   --  metalog_open
   --------------------------------------------------------------------------------------------
   function metalog_open (filename : String) return Pkg_Error_Type is
   begin
      if DIR.Exists (filename) then
         TIO.Create (File => metalog_handle,
                     Mode => TIO.Out_File,
                     Name => filename);
      else
         TIO.Open (File => metalog_handle,
                   Mode => TIO.Append_File,
                   Name => filename);
      end if;
      return EPKG_OK;
   exception
      when others =>
         EV.pkg_emit_with_strerror (SUS ("Unable to open metalog '" & filename & "'"));
         return EPKG_FATAL;
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
      function print_flags_label return String;
      function print_flags_label return String is
      begin
         if fflags > 0 then
            return " flags=";
         else
            return "";
         end if;
      end print_flags_label;
   begin
      if not TIO.Is_Open (metalog_handle) then
         return;
      end if;

      declare
         fflags_label : String := print_flags_label;
         fflags_buf   : String := "tbd";
         mode_str     : String := "??";
      begin
         case mtype is
            when PKG_METALOG_DIR =>
               TIO.Put_Line (metalog_handle, "./" & path &
                               " type=dir " &
                               " uname=" & uname &
                               " gname=" & gname &
                               " mode=" & mode_str & fflags_label & fflags_buf);
            when PKG_METALOG_FILE =>
               TIO.Put_Line (metalog_handle, "./" & path &
                               " type=file " &
                               " uname=" & uname &
                               " gname=" & gname &
                               " mode=" & mode_str & fflags_label & fflags_buf);
            when PKG_METALOG_LINK =>
               TIO.Put_Line (metalog_handle, "./" & path &
                               " type=link " &
                               " uname=" & uname &
                               " gname=" & gname &
                               " mode=" & mode_str &
                               " link=" & link & fflags_label & fflags_buf);
         end case;
      exception
         when others =>
            EV.pkg_emit_with_strerror (SUS ("Unable to write to the metalog"));
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


end Core.Metalog;
