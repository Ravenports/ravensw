--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Core.Event;

package body Core.Version is

   package EV renames Core.Event;

   --------------------------------------------------------------------
   --  split_version
   --------------------------------------------------------------------
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
            ver_wo_epoch := SUS (head (versionstr, ","));
            begin
               result.epoch :=
                 Natural (Integer'Value (specific_field (versionstr, count_commas + 1, ",")));
            exception
               when Constraint_Error =>
                  result.epoch := 0;
            end;
         end if;

         declare
            workstr : String := USS (ver_wo_epoch);
            count_scores : Natural := count_char (workstr, '_');
         begin
            if count_scores = 0 then
               --  No revision
               result.revision := 0;
               result.version := SUS (workstr);
            else
               result.version  := SUS (head (versionstr, "_"));
               begin
                  result.revision :=
                 Natural (Integer'Value (specific_field (versionstr, count_scores + 1, "_")));
               exception
                  when Constraint_Error =>
                     result.revision := 0;
               end;
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


   --------------------------------------------------------------------
   --  pkg_version_cmp
   --------------------------------------------------------------------
   function pkg_version_cmp (pkg1, pkg2 : String) return cmp_result
   is
      function distinguish (n1, n2 : Integer) return cmp_result;

      v1 : version_breakdown;
      v2 : version_breakdown;

      identical     : constant cmp_result :=  0;
      pkg1_is_older : constant cmp_result := -1;
      pkg1_is_newer : constant cmp_result :=  1;

      result : cmp_result := identical;

      function distinguish (n1, n2 : Integer) return cmp_result is
      begin
         if n1 = n2 then
            return identical;
         elsif n1 < n2 then
            return pkg1_is_older;
         else
            return pkg1_is_newer;
         end if;
      end distinguish;
   begin
      v1 := split_version (pkg1);
      v2 := split_version (pkg2);

      --  Check epoch, port version, and port revision, in that order.
      result := distinguish (v1.epoch, v2.epoch);

      --  Check the version if packages have the same epoch
      if result = identical then
         if not equivalent (v1.version, v2.version) then
            declare
               version1 : String := USS (v1.version);
               version2 : String := USS (v2.version);
               v1_index : Integer := version1'First;
               v2_index : Integer := version2'First;
            begin
               --  Loop over different components (the parts separated by dots).
               --  If any component differs, there is an inequality.
               loop
                  exit when result /= identical;
                  exit when v1_index > version1'Last and then v2_index > version2'Last;

                  declare
                     block_v1 : Boolean;
                     block_v2 : Boolean;
                     vc1      : version_component;
                     vc2      : version_component;
                  begin
                     if v1_index > version1'Last or else version1 (v1_index) = '+' then
                        block_v1 := True;
                     else
                        vc1 := get_component (version1, v1_index);
                     end if;
                     if v2_index > version2'Last or else version2 (v2_index) = '+' then
                        block_v2 := True;
                     else
                        vc2 := get_component (version2, v2_index);
                     end if;
                     if block_v1 and then block_v2 then
                        if v1_index < version1'Last then
                           v1_index := v1_index + 1;
                        end if;
                        if v2_index < version2'Last then
                           v2_index := v2_index + 1;
                        end if;
                     elsif vc1.n /= vc2.n then
                        result := distinguish (Integer (vc1.n), Integer (vc2.n));
                     elsif vc1.a /= vc2.a then
                        result := distinguish (vc1.a, vc2.a);
                     elsif vc1.pl /= vc2.pl then
                        result := distinguish (Integer (vc1.pl), Integer (vc2.pl));
                     end if;
                  end;
               end loop;
            end;
         end if;
      end if;

      --  Compare revision numbers.
      if result = identical then
         result := distinguish (v1.revision, v2.revision);
      end if;

      return result;
   end pkg_version_cmp;


   --------------------------------------------------------------------
   --  get_component
   --------------------------------------------------------------------
   function get_component
     (full_version : in String;
      position     : in out Integer) return version_component
   is
      has_stage       : Boolean := False;
      has_patch_level : Boolean := False;
      component       : version_component;
   begin
      --  Handle version number
      if is_digit (full_version (position)) then
         component.n := pull_leading_number (full_version, position);
      elsif full_version (position) = '*' then
         component.n := -2;
         loop
            position := position + 1;
            exit when position > full_version'Last;
            exit when full_version (position) /= '+';
         end loop;
      else
         component.n := -1;
         has_stage := True;
      end if;

      if position > full_version'Last then
         return component;
      end if;

      --  handle letter
      if is_alpha (full_version (position)) then
         declare
            c    : Character := to_lower (full_version (position));
            done : Character := Character'Val (0);
         begin
            has_patch_level := True;

            --  special prefixes
            if position <= full_version'Last and then
              is_alpha (full_version (position))
            then
               for S in 1 .. stage'Length loop
                  if position + stage (S).namelen - 1 <= full_version'Last then
                     if full_version (position .. position + stage (S).namelen - 1) =
                       stage (S).name (1 .. stage (S).namelen)
                     then
                        if position + stage (S).namelen > full_version'Last or else
                          not is_alpha (full_version (position + stage (S).namelen))
                        then
                           if has_stage then
                              --  stage to value
                              component.a := stage (S).value;
                              position := position + stage (S).namelen;
                           else
                              --  insert dot
                              component.a := 0;
                              has_patch_level := False;
                           end if;
                           c := done;
                           exit;
                        end if;
                     end if;
                  end if;
               end loop;
            end if;

            --  unhandled above
            if c /= done then
               --  use the first letter and skip following
               component.a := Character'Pos (c) - Character'Pos ('a') + 1;
               loop
                  position := position + 1;
                  exit when position > full_version'Last;
                  exit when not is_alpha (full_version (position));
               end loop;
            end if;
         end;
      else
         component.a := 0;
         has_patch_level := False;
      end if;

      if position > full_version'Last then
         return component;
      end if;

      if has_patch_level then
         --  handle patch number
         if is_digit (full_version (position)) then
            component.pl := pull_leading_number (full_version, position);
         else
            component.pl := -1;
         end if;
      else
         component.pl := 0;
      end if;

      if position > full_version'Last then
         return component;
      end if;

      --  skip trailing separators
      loop
         exit when position > full_version'Last;
         exit when is_digit (full_version (position)) or else
           is_alpha (full_version (position)) or else
           full_version (position) = '*' or else
           full_version (position) = '+';
         position := position + 1;
      end loop;

      return component;
   end get_component;


   --------------------------------------------------------------------
   --  is_digit
   --------------------------------------------------------------------
   function is_digit (x : Character) return Boolean is
   begin
      case x is
         when '0' .. '9' => return True;
         when others => return False;
      end case;
   end is_digit;


   --------------------------------------------------------------------
   --  is_alpha
   --------------------------------------------------------------------
   function is_alpha (x : Character) return Boolean is
   begin
      case x is
         when 'A' .. 'Z' => return True;
         when 'a' .. 'z' => return True;
         when others => return False;
      end case;
   end is_alpha;


   --------------------------------------------------------------------
   --  to_lower
   --------------------------------------------------------------------
   function to_lower (x : Character) return Character
   is
      diff : constant Integer := Character'Pos ('a') - Character'Pos ('A');
   begin
      case x is
         when 'A' .. 'Z' =>
            return Character'Val (Character'Pos (x) + diff);
         when others =>
            return x;
      end case;
   end to_lower;


   --------------------------------------------------------------------
   --  pull_leading_number
   --------------------------------------------------------------------
   function pull_leading_number
     (full_version : in String;
      position     : in out Integer) return vc_int64
   is
      number_digits : Natural := 0;
      look_ahead    : Integer := position;
   begin
      loop
         exit when look_ahead > full_version'Last;
         if is_digit (full_version (look_ahead)) then
            number_digits := number_digits + 1;
            look_ahead := look_ahead + 1;
         else
            exit;
         end if;
      end loop;
      if number_digits = 0 then
         return 0;  --  this should not happen, ever
      end if;

      declare
         fragment : constant String (1 .. number_digits) :=
           full_version (position .. position + number_digits - 1);
      begin
         position := position + number_digits;
         return vc_int64 (Integer'Value (fragment));
      exception
         when others =>
            EV.pkg_emit_error (SUS ("exception hit in pull_leading_number(): " & fragment));
            return 0;
      end;
   end pull_leading_number;


   --------------------------------------------------------------------
   --  pkg_version_change
   --------------------------------------------------------------------
   function pkg_version_change (pkg : access T_pkg) return T_pkg_change is
   begin
      if IsBlank (pkg.old_version) then
         return PKG_REINSTALL;
      end if;

      case pkg_version_cmp (USS (pkg.old_version), USS (pkg.version)) is
         when -1 => return PKG_UPGRADE;
         when  0 => return PKG_REINSTALL;
         when  1 => return PKG_DOWNGRADE;
      end case;
   end pkg_version_change;


   --------------------------------------------------------------------
   --  pkg_version_change_between
   --------------------------------------------------------------------
   function pkg_version_change_between (pkg1, pkg2 : access T_pkg) return T_pkg_change
   is
   begin
      if pkg2 = null then
         return PKG_REINSTALL;
      end if;

      case pkg_version_cmp (USS (pkg2.version), USS (pkg1.version)) is
         when -1 => return PKG_UPGRADE;
         when  0 => return PKG_REINSTALL;
         when  1 => return PKG_DOWNGRADE;
      end case;
   end pkg_version_change_between;


end Core.Version;
