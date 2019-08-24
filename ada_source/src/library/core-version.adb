--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Core.Event;

package body Core.Version is

   package EV renames Core.Event;

   function split_version (pkgname : String) return version_breakdown
   is
      --  ${PORTNAME}-${PORTVERSION}[_${PORTREVISION}][,${PORTEPOCH}]
      function get_versionstr return String;
      procedure split_versionstr (versionstr : String);

      result : version_breakdown;

      function get_versionstr return String
      is
         count : Natural;
      begin
         --  Look for the last '-' the the pkgname
         count := count_char (pkgname, '-');
         if count = 0 then
            --  Cheat if we are just passed a version, not a valid package name
            return pkgname;
         else
            return specific_field (pkgname, count + 1, "-");
         end if;
      end get_versionstr;

      procedure split_versionstr (versionstr : String)
      is
         count_commas : Natural := count_char (versionstr, ',');
         ver_wo_epoch : Text;
      begin
         if count_commas = 0 then
            --  No epoch
            result.epoch := 0;
            ver_wo_epoch := SUS (versionstr);
         else
            result.epoch :=
              Natural (Integer'Value (specific_field (versionstr, count_commas + 1, ",")));
            ver_wo_epoch := SUS (head (versionstr, ","));
         end if;

         declare
            workstr : String := USS (ver_wo_epoch);
            count_scores : Natural := count_char (workstr, '_');
         begin
            if count_scores = 0 then
               --  No revision
               result.revision := 0;
               result.endname := SUS (workstr);
            else
               result.revision :=
                 Natural (Integer'Value (specific_field (versionstr, count_commas + 1, "_")));
               result.endname  := SUS (head (versionstr, "_"));
            end if;
         end;
      end split_versionstr;

      versionstr : String := get_versionstr;
   begin
      if IsBlank (pkgname) then
         EV.pkg_emit_error (SUS ("split_version: passed empty string for pkgname"));
         return result;
      end if;

      split_versionstr (versionstr);
      return result;
   end split_version;

end Core.Version;
