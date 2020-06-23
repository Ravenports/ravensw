--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Exceptions;
with System;
with Interfaces;

with Core.Event;
with Core.Utilities;
with Core.Unix;
with Core.Strings;
with Core.Pkg_Operations;

use Core.Strings;

package body Core.Manifest is

   package POP renames Core.Pkg_Operations;

   --------------------------------------------------------------------
   --  url_decode
   --------------------------------------------------------------------
   function url_decode (original : String) return String
   is
      --  output is same or smaller than original
      result : String (1 .. original'Length) := (others => ' ');
      len    : Natural := 0;
      arrow  : Natural := original'First;
      value  : Natural := 0;
   begin
      loop
         exit when arrow > original'Last;
         if original (arrow) = '%' then
            --  we expect 2 more characters.  If we don't have them, just skip
            exit when arrow + 2 > original'Last;
            len := len + 1;
            result (len) := Utilities.hex2char (original (arrow + 1 .. arrow + 2));
            if result (len) = Character'Val (0) then
               --  hex conversion failed, so don't consider this hex and just copy verbatim
               result (len)     := original (arrow);
               result (len + 1) := original (arrow + 1);
               result (len + 2) := original (arrow + 2);
               len := len + 2;
            end if;
            arrow := arrow + 3;
         else
            len := len + 1;
            result (len) := original (arrow);
            arrow := arrow + 1;
         end if;
      end loop;
      return result (1 .. len);
   end url_decode;


   --------------------------------------------------------------------
   --  pkg_string
   --------------------------------------------------------------------
   function pkg_string
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result
   is
      function get_value return String;
      function get_value return String is
      begin
         case field is
            when version =>
               if Ucl.type_is_string (obj'Access) then
                  return Ucl.ucl_object_tostring (obj'Access);
               else
                  return Ucl.ucl_object_tostring_forced (obj'Access);
               end if;
            when others =>
               return Ucl.ucl_object_tostring (obj'Access);
         end case;
      end get_value;

      str : String := get_value;
   begin
      if IsBlank (str) then
         Event.emit_debug (3, field'Img & " field is blank (" & USS (pkg_access.name) & ")");
      end if;
      case field is
         when liclogic =>
            if str = "single" then
               pkg_access.licenselogic := Pkgtypes.LICENSE_SINGLE;
            elsif str = "or" or else str = "dual" then
               pkg_access.licenselogic := Pkgtypes.LICENSE_OR;
            elsif str = "and" or else str = "multi" then
               pkg_access.licenselogic := Pkgtypes.LICENSE_AND;
            else
               Event.emit_error ("Unknown license logic: " & str);
               return RESULT_FATAL;
            end if;
         when description =>
            pkg_access.desc := SUS (url_decode (str));
         when abi =>
            pkg_access.abi := SUS (str);
         when arch =>
            pkg_access.arch := SUS (str);
         when checksum =>
            pkg_access.sum := SUS (str);
         when comment =>
            pkg_access.comment := SUS (str);
         when maintainer =>
            pkg_access.maintainer := SUS (str);
         when name =>
            pkg_access.name := SUS (str);
         when origin =>
            pkg_access.origin := SUS (str);
         when prefix =>
            pkg_access.prefix := SUS (str);
         when repopath =>
            pkg_access.repopath := SUS (str);
         when www =>
            pkg_access.www := SUS (str);
         when version =>
            pkg_access.version := SUS (str);
         when path =>
            --  duplicate of repopath with FreeBSD's pkg(8), ignore
            null;
         when others =>
            Event.emit_error ("Developer failure, not a string : " & field'Img);
            return RESULT_FATAL;
      end case;
      return RESULT_OK;
   end pkg_string;


   --------------------------------------------------------------------
   --  pkg_int
   --------------------------------------------------------------------
   function pkg_int
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result
   is
      dest : constant int64 := Ucl.ucl_object_toint (obj'Access);
   begin
      case field is
         when flatsize =>
            pkg_access.flatsize := Pkgtypes.Package_Size (dest);

         when pkgsize =>
            pkg_access.pkgsize := Pkgtypes.Package_Size (dest);

         when others =>
            Event.emit_error ("Developer failure, not an integer : " & field'Img);
            return RESULT_FATAL;
      end case;
      return RESULT_OK;
   end pkg_int;


   --------------------------------------------------------------------
   --  pkg_boolean
   --------------------------------------------------------------------
   function pkg_boolean
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result
   is
      dest : constant Boolean := Ucl.ucl_object_toboolean (obj'Access);
   begin
      case field is
         when vital =>
            pkg_access.vital := dest;

         when others =>
            Event.emit_error ("Developer failure, not a boolean : " & field'Img);
            return RESULT_FATAL;
      end case;

      return RESULT_OK;
   end pkg_boolean;


   --------------------------------------------------------------------
   --  pkg_obj
   --------------------------------------------------------------------
   function pkg_obj
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result
   is
      procedure setres (result : Action_Result);

      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      item : access constant libucl.ucl_object_t;
      rc   : Action_Result := RESULT_OK;

      procedure setres (result : Action_Result) is
      begin
         if result /= RESULT_OK then
            rc := RESULT_FATAL;
         end if;
      end setres;
   begin
      Event.emit_debug (3, "Manifest: parsing object");
      loop
         item := Ucl.ucl_object_iterate (obj'Access, iter'Access, True);
         exit when item = null;

         declare
            key : constant String := Ucl.ucl_object_key (item);
            keytxt : Text := SUS (key);
         begin
            if not IsBlank (key) then
               case field is

               when annotations =>
                  if not Ucl.type_is_string (item) then
                     Event.emit_error ("Skipping malformed annotation " & key);
                  else
                     if pkg_access.annotations.Contains (keytxt) then
                        Event.emit_notice ("Skipping duplicate annotation key " & key);
                     else
                        pkg_access.annotations.Insert
                          (keytxt, SUS (Ucl.ucl_object_tostring (item)));
                     end if;
                  end if;

               when options =>
                  if not Ucl.type_is_string (item) and then
                    not Ucl.type_is_boolean (item)
                  then
                     Event.emit_error ("Skipping malformed option " & key);
                  else
                     if pkg_access.options.Contains (keytxt) then
                        Event.emit_notice ("Skipping duplicate option key " & key);
                     else
                        if Ucl.type_is_boolean (item) then
                           if Ucl.ucl_object_toboolean (item) then
                              pkg_access.options.Insert (keytxt, SUS ("on"));
                           else
                              pkg_access.options.Insert (keytxt, SUS ("off"));
                           end if;
                        else
                           pkg_access.options.Insert
                             (keytxt, SUS (Ucl.ucl_object_tostring (item)));
                        end if;
                     end if;
                  end if;

               when directories =>
                  if Ucl.type_is_boolean (item) then
                     setres (POP.pkg_adddir (pkg_access, SUS (url_decode (key)), False));
                  elsif Ucl.type_is_string (item) then
                     setres (POP.pkg_adddir (pkg_access, SUS (url_decode (key)), False));
                  elsif Ucl.type_is_object (item) then
                     setres (pkg_set_dirs_from_object (pkg_access, item));
                  else
                     Event.emit_error ("Skipping malformed directories " & key);
                  end if;

               when pkg_dirs =>
                  if Ucl.type_is_object (item) then
                     setres (pkg_set_dirs_from_object (pkg_access, item));
                  else
                     Event.emit_error ("Skipping malformed dirs  " & key);
                  end if;

               when files =>
                  if Ucl.type_is_string (item) then
                     declare
                        chksum : String := Ucl.ucl_object_tostring (item);
                     begin
                        if chksum'Length > 1 then
                           setres (POP.pkg_addfile (pkg_access       => pkg_access,
                                                    path             => SUS (url_decode (key)),
                                                    sum              => SUS (chksum),
                                                    check_duplicates => False));
                        else
                           setres (POP.pkg_addfile (pkg_access       => pkg_access,
                                                    path             => SUS (url_decode (key)),
                                                    sum              => blank,
                                                    check_duplicates => False));
                        end if;
                     end;
                  elsif Ucl.type_is_object (item) then
                     setres (pkg_set_files_from_object (pkg_access, item));
                  else
                     Event.emit_error ("Skipping malformed files " & key);
                  end if;

               when scripts =>
                  if not Ucl.type_is_string (item) then
                     Event.emit_error ("Skipping malformed script " & key);
                  else
                     declare
                        valid   : Boolean;
                        stype   : Pkgtypes.Package_Script;
                        payload : String := Ucl.ucl_object_tostring (item);
                     begin
                        stype := script_type (key, valid);
                        if valid then
                           setres (POP.pkg_addscript (pkg_access => pkg_access,
                                                      data       => SUS (url_decode (payload)),
                                                      script     => stype));
                        else
                           Event.emit_error ("Skipping unknown script type: " & key);
                        end if;
                     end;
                  end if;

               when deps =>
                  if not Ucl.type_is_object (item) and then
                    not Ucl.type_is_array (item)
                  then
                     Event.emit_error ("Skipping malformed dependency " & key);
                  else
                     setres (pkg_set_deps_from_object (pkg_access, item));
                  end if;

               when others =>
                  Event.emit_error ("Developer failure, not an object : " & field'Img);
               end case;
            end if;
         end;
      end loop;

      return rc;
   end pkg_obj;


   --------------------------------------------------------------------
   --  script_type
   --------------------------------------------------------------------
   function script_type
     (scrtype_string : String;
      valid : out Boolean) return Pkgtypes.Package_Script is
   begin
      valid := True;
      if scrtype_string = "pre-install" then
         return Pkgtypes.PKG_SCRIPT_PRE_INSTALL;
      elsif scrtype_string = "install" then
         return Pkgtypes.PKG_SCRIPT_INSTALL;
      elsif scrtype_string = "post-install" then
         return Pkgtypes.PKG_SCRIPT_POST_INSTALL;
      elsif scrtype_string = "pre-upgrade" then
         return Pkgtypes.PKG_SCRIPT_PRE_UPGRADE;
      elsif scrtype_string = "upgrade" then
         return Pkgtypes.PKG_SCRIPT_UPGRADE;
      elsif scrtype_string = "post-upgrade" then
         return Pkgtypes.PKG_SCRIPT_POST_UPGRADE;
      elsif scrtype_string = "pre-deinstall" then
         return Pkgtypes.PKG_SCRIPT_PRE_DEINSTALL;
      elsif scrtype_string = "deinstall" then
         return Pkgtypes.PKG_SCRIPT_DEINSTALL;
      elsif scrtype_string = "post-deinstall" then
         return Pkgtypes.PKG_SCRIPT_POST_DEINSTALL;
      else
         valid := False;
         return Pkgtypes.Package_Script'First;
      end if;
   end script_type;


   --------------------------------------------------------------------
   --  pkg_set_dirs_from_object
   --------------------------------------------------------------------
   function pkg_set_dirs_from_object
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : access constant libucl.ucl_object_t) return Action_Result
   is
      okey  : constant String := Ucl.ucl_object_key (obj);
      uname : Text;
      gname : Text;
      perm  : Pkgtypes.mode_t;
   begin
      if IsBlank (okey) then
         return RESULT_FATAL;
      end if;

      declare
         dirname : constant String := url_decode (okey);
         iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
         cur  : access constant libucl.ucl_object_t;
      begin
         loop
            cur := Ucl.ucl_object_iterate (obj, iter'Access, True);
            exit when cur = null;
            declare
               ckey : constant String := Ucl.ucl_object_key (cur);
            begin
               if not IsBlank (ckey) then
                  if ckey = "uname" then
                     if Ucl.type_is_string (cur) then
                        uname := SUS (Ucl.ucl_object_tostring (cur));
                     end if;
                  elsif ckey = "gname" then
                     if Ucl.type_is_string (cur) then
                        gname := SUS (Ucl.ucl_object_tostring (cur));
                     end if;
                  elsif ckey = "perm" then
                     if Ucl.type_is_string (cur) or else
                       Ucl.type_is_integer (cur)
                     then
                        declare
                           val : constant String := Ucl.ucl_object_tostring_forced (cur);
                           res : constant Integer := Unix.get_mode (val);
                        begin
                           if res < 0 then
                              Event.emit_error ("Not a valid mode: " & val);
                           else
                              perm := Pkgtypes.mode_t (res);
                           end if;
                        end;
                     end if;
                  else
                     Event.emit_debug
                       (1, "Skipping unknown key for dir(" & dirname & "): " & ckey);
                  end if;
               end if;
            end;
         end loop;
         return POP.pkg_adddir_attr (pkg_access       => pkg_access,
                                     path             => SUS (dirname),
                                     uname            => uname,
                                     gname            => gname,
                                     perm             => perm,
                                     fflags           => 0,
                                     check_duplicates => False);
      end;
   end pkg_set_dirs_from_object;


   --------------------------------------------------------------------
   --  pkg_set_files_from_object
   --------------------------------------------------------------------
   function pkg_set_files_from_object
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : access constant libucl.ucl_object_t) return Action_Result
   is
      okey  : constant String := Ucl.ucl_object_key (obj);
      uname : Text;
      gname : Text;
      cksum : Text;
      perm  : Pkgtypes.mode_t;
   begin
      if IsBlank (okey) then
         return RESULT_FATAL;
      end if;

      declare
         fname : constant String := url_decode (okey);
         iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
         cur  : access constant libucl.ucl_object_t;
      begin
         loop
            cur := Ucl.ucl_object_iterate (obj, iter'Access, True);
            exit when cur = null;
            declare
               ckey : constant String := Ucl.ucl_object_key (cur);
            begin
               if not IsBlank (ckey) then
                  if ckey = "uname" then
                     if Ucl.type_is_string (cur) then
                        uname := SUS (Ucl.ucl_object_tostring (cur));
                     end if;
                  elsif ckey = "gname" then
                     if Ucl.type_is_string (cur) then
                        gname := SUS (Ucl.ucl_object_tostring (cur));
                     end if;
                  elsif ckey = "sum" then
                     if Ucl.type_is_string (cur) then
                        declare
                           val : constant String := Ucl.ucl_object_tostring (cur);
                        begin
                           if val'Length = 64 then
                              cksum := SUS (val);
                           end if;
                        end;
                     end if;
                  elsif ckey = "perm" then
                     if Ucl.type_is_string (cur) or else
                       Ucl.type_is_integer (cur)
                     then
                        declare
                           val : constant String := Ucl.ucl_object_tostring_forced (cur);
                           res : constant Integer := Unix.get_mode (val);
                        begin
                           if res < 0 then
                              Event.emit_error ("Not a valid mode: " & val);
                           else
                              perm := Pkgtypes.mode_t (res);
                           end if;
                        end;
                     end if;
                  else
                     Event.emit_debug (1, "Skipping unknown key for file(" & fname & "): " & ckey);
                  end if;
               end if;
            end;
         end loop;
         return POP.pkg_addfile_attr (pkg_access       => pkg_access,
                                      path             => SUS (fname),
                                      uname            => uname,
                                      gname            => gname,
                                      perm             => perm,
                                      fflags           => 0,
                                      sum              => cksum,
                                      check_duplicates => False);
      end;
   end pkg_set_files_from_object;


   --------------------------------------------------------------------
   --  pkg_set_deps_from_object
   --------------------------------------------------------------------
   function pkg_set_deps_from_object
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : access constant libucl.ucl_object_t) return Action_Result
   is
      okey    : constant String := Ucl.ucl_object_key (obj);
      version : Text;
      origin  : Text;
      result  : Action_Result := RESULT_OK;
      expval  : Boolean := Ucl.type_is_array (obj);
   begin
      if IsBlank (okey) then
         return RESULT_FATAL;
      end if;

      Event.emit_debug (4, "Found " & okey);
      declare
         iter : aliased libucl.ucl_object_iter_t :=
           libucl.ucl_object_iter_t (System.Null_Address);
         self : access constant libucl.ucl_object_t;
      begin
         loop
            self := Ucl.ucl_object_iterate (obj, iter'Access, expval);
            exit when self = null;
            declare
               iter2 : aliased libucl.ucl_object_iter_t :=
                 libucl.ucl_object_iter_t (System.Null_Address);
               cur   : access constant libucl.ucl_object_t;
            begin
               loop
                  cur := Ucl.ucl_object_iterate (self, iter2'Access, True);
                  exit when cur = null;
                  declare
                     ckey : constant String := Ucl.ucl_object_key (cur);
                  begin
                     if ckey = "origin" then
                        if Ucl.type_is_string (cur) then
                           origin := SUS (Ucl.ucl_object_tostring (cur));
                        end if;
                     elsif ckey = "version" then
                        if Ucl.type_is_string (cur) then
                           version := SUS (Ucl.ucl_object_tostring (cur));
                        else
                           version := SUS (Ucl.ucl_object_tostring_forced (cur));
                        end if;
                     end if;
                  end;
               end loop;
               if IsBlank (origin) then
                  Event.emit_error ("Skipping malformed dependency " & okey);
               else
                  if IsBlank (version) then
                     Event.emit_error ("dependency " & USS (origin) & " has no version");
                     result := RESULT_FATAL;
                  else
                     if POP.pkg_adddep (pkg_access => pkg_access,
                                        name       => SUS (okey),
                                        origin     => origin,
                                        version    => version,
                                        locked     => False) /= RESULT_OK
                     then
                        result := RESULT_FATAL;
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end;
      return result;
   end pkg_set_deps_from_object;


   --------------------------------------------------------------------
   --  pkg_array
   --------------------------------------------------------------------
   function pkg_array
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result
   is
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      item : access constant libucl.ucl_object_t;
      result : Action_Result := RESULT_OK;
   begin
      Event.emit_debug (3, "Manifest: parsing array");
      loop
         item := Ucl.ucl_object_iterate (obj'Access, iter'Access, True);
         exit when item = null;

         Event.emit_debug (4, field'Img);
         case field is
            when categories =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addstring (crate => pkg_access.categories,
                                        data  => Ucl.ucl_object_tostring (item),
                                        title => "category") /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed category");
               end if;

            when licenses =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addstring (crate => pkg_access.licenses,
                                        data  => Ucl.ucl_object_tostring (item),
                                        title => "license") /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed license");
               end if;

            when pkg_users =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addstring (crate => pkg_access.users,
                                        data  => Ucl.ucl_object_tostring (item),
                                        title => "user") /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               elsif Ucl.type_is_object (item) then
                  if pkg_obj (pkg_access => pkg_access,
                              obj        => item.all,
                              field      => field) /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed users");
               end if;

            when pkg_groups =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addstring (crate => pkg_access.groups,
                                        data  => Ucl.ucl_object_tostring (item),
                                        title => "group") /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               elsif Ucl.type_is_object (item) then
                  if pkg_obj (pkg_access => pkg_access,
                              obj        => item.all,
                              field      => field) /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed groups");
               end if;


            when pkg_shlibs_reqd =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addshlib_required
                    (pkg_access => pkg_access,
                     data       => Ucl.ucl_object_tostring (item)) /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed required shared library");
               end if;

            when pkg_shlibs_prov =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addshlib_provided
                    (pkg_access => pkg_access,
                     data       => Ucl.ucl_object_tostring (item)) /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed provided shared library");
               end if;

            when pkg_conflicts =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addconflict
                    (pkg_access => pkg_access,
                     data       => Ucl.ucl_object_tostring (item)) /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed conflict name");
               end if;

            when pkg_config_files =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addconfig_file
                    (pkg_access => pkg_access,
                     path       => SUS (Ucl.ucl_object_tostring (item)),
                     content    => blank) /= RESULT_OK
                  then
                     return RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed config file name");
               end if;

            when pkg_requires =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addstring_silent_unique
                    (crate => pkg_access.requires,
                     data  => Ucl.ucl_object_tostring (item)) /= RESULT_OK
                  then
                     return RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed require name");
               end if;

            when pkg_provides =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_addstring_silent_unique
                    (crate => pkg_access.provides,
                     data  => Ucl.ucl_object_tostring (item)) /= RESULT_OK
                  then
                     return RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed provide name");
               end if;

            when pkg_dirs =>
               if Ucl.type_is_string (item) then
                  if POP.pkg_adddir (pkg_access       => pkg_access,
                                     path             => SUS (Ucl.ucl_object_tostring (item)),
                                     check_duplicates => False) /= RESULT_OK
                  then
                     result := RESULT_FATAL;
                  end if;
               elsif Ucl.type_is_object (item) then
                  if pkg_obj (pkg_access, item.all, field) /= RESULT_OK then
                     result := RESULT_FATAL;
                  end if;
               else
                  Event.emit_error ("Skipping malformed dirs");
               end if;

            when others =>
               Event.emit_error ("Developer failure, not an array : " & field'Img);
         end case;
      end loop;
      return result;
   end pkg_array;


   --------------------------------------------------------------------
   --  pkg_message
   --------------------------------------------------------------------
   function pkg_message
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result
   is
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      cur  : access constant libucl.ucl_object_t;
      elt  : access constant libucl.ucl_object_t;
   begin
      case field is
         when messages =>
            if Ucl.type_is_array (obj'Access) then
               --  New format of pkg message
               loop
                  cur := Ucl.ucl_object_iterate (obj'Access, iter'Access, True);
                  exit when cur = null;

                  elt := Ucl.ucl_object_find_key (cur, "message");
                  if not Ucl.type_is_string (elt) then
                     Event.emit_error ("package message lacks required 'message' key");
                     return RESULT_FATAL;
                  end if;

                  declare
                     msg  : Pkgtypes.Package_Message;
                     elt2 : access constant libucl.ucl_object_t;
                     elt3 : access constant libucl.ucl_object_t;
                     elt4 : access constant libucl.ucl_object_t;
                  begin
                     msg.message_type := Pkgtypes.PKG_MESSAGE_ALWAYS;
                     msg.contents := SUS (Ucl.ucl_object_tostring (elt));
                     elt2 := Ucl.ucl_object_find_key (cur, "type");
                     if Ucl.type_is_string (elt2) then
                        declare
                           lowstr : constant String := lowercase (Ucl.ucl_object_tostring (elt2));
                        begin
                           if lowstr = "install" then
                              msg.message_type := Pkgtypes.PKG_MESSAGE_INSTALL;
                           elsif lowstr = "remove" then
                              msg.message_type := Pkgtypes.PKG_MESSAGE_REMOVE;
                           elsif lowstr = "upgrade" then
                              msg.message_type := Pkgtypes.PKG_MESSAGE_UPGRADE;
                           else
                              Event.emit_error
                                ("Unknown message type so message is always printed");
                           end if;
                        end;
                     end if;
                     case msg.message_type is
                        when Pkgtypes.PKG_MESSAGE_UPGRADE =>
                           elt3 := Ucl.ucl_object_find_key (cur, "minimum_version");
                           if Ucl.type_is_string (elt3) then
                              msg.minimum_version := SUS (Ucl.ucl_object_tostring (elt3));
                           end if;
                           elt4 := Ucl.ucl_object_find_key (cur, "maximum_version");
                           if Ucl.type_is_string (elt4) then
                              msg.maximum_version := SUS (Ucl.ucl_object_tostring (elt3));
                           end if;
                        when Pkgtypes.PKG_MESSAGE_ALWAYS
                           | Pkgtypes.PKG_MESSAGE_INSTALL
                           | Pkgtypes.PKG_MESSAGE_REMOVE =>
                           null;
                     end case;
                     pkg_access.messages.Append (msg);
                  end;
               end loop;
            else
               Event.emit_error ("package message was badly formatted (array was expected)");
            end if;

         when others =>
            Event.emit_error ("Developer failure, not a message : " & field'Img);

            return RESULT_FATAL;
      end case;
      return RESULT_OK;
   end pkg_message;


   --------------------------------------------------------------------
   --  parse_manifest
   --------------------------------------------------------------------
   function parse_manifest
     (pkg_access : Pkgtypes.A_Package_Access;
      manifest   : String) return Action_Result
   is
      parser : Ucl.T_parser;
      obj    : access libucl.ucl_object_t;
      rc     : Action_Result;
   begin
      Event.emit_debug (2, "Parsing manifest line : " & pad_right (manifest, 40));

      parser := Ucl.ucl_parser_new_nofilevars;

      if not Ucl.ucl_parser_add_chunk (parser, manifest) then
         Event.emit_error ("Error parsing manifest: " & Ucl.ucl_parser_get_error (parser));
         libucl.ucl_parser_free (parser);
         return RESULT_FATAL;
      end if;

      obj := Ucl.ucl_parser_get_object (parser);
      libucl.ucl_parser_free (parser);

      if obj = null then
         Event.emit_error ("parse_manifest: failed ucl_parser_get_object");
         Event.emit_debug (1, manifest);
         return RESULT_FATAL;
      end if;

      rc := pkg_parse_manifest_ucl (pkg_access, obj);
      libucl.ucl_object_unref (obj);

      return rc;
   exception
      when oops : others =>
         Event.emit_error ("Unexpected exception caught at parse_manifest");
         Event.emit_error (Ada.Exceptions.Exception_Information (oops));
         return RESULT_FATAL;
   end parse_manifest;


   --------------------------------------------------------------------
   --  pkg_parse_manifest_ucl
   --------------------------------------------------------------------
   function pkg_parse_manifest_ucl
     (pkg_access : Pkgtypes.A_Package_Access;
      obj : access constant libucl.ucl_object_t) return Action_Result
   is
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      cur  : access constant libucl.ucl_object_t;
      rc   : Action_Result := RESULT_OK;
   begin
      loop
         cur := Ucl.ucl_object_iterate (obj, iter'Access, True);
         exit when cur = null;

         declare
            key : String := Ucl.ucl_object_key (cur);
            field : pkg_field := get_field (key);
         begin
            case field is
               when NOTFOUND =>
                  Event.emit_notice ("Unrecognized manifest key: " & key);

               when annotations |
                    directories |
                    options     |
                    scripts     |
                    files       |
                    deps =>
                  if pkg_obj (pkg_access, cur.all, field) /= RESULT_OK then
                     rc := RESULT_FATAL;
                  end if;

               when description    |
                    maintainer     |
                    repopath       |
                    checksum       |
                    liclogic       |
                    comment        |
                    version        |
                    origin         |
                    prefix         |
                    path           |
                    arch           |
                    name           |
                    abi            |
                    www =>
                  if pkg_string (pkg_access, cur.all, field) /= RESULT_OK then
                     rc := RESULT_FATAL;
                  end if;

               when pkg_config_files |
                    pkg_conflicts    |
                    pkg_shlibs_reqd  |
                    pkg_shlibs_prov  |
                    pkg_requires     |
                    pkg_provides     |
                    categories       |
                    pkg_groups       |
                    pkg_users        |
                    pkg_dirs         |
                    licenses =>
                  if pkg_array (pkg_access, cur.all, field)  /= RESULT_OK then
                     rc := RESULT_FATAL;
                  end if;

               when flatsize |
                    pkgsize =>
                  if pkg_int (pkg_access, cur.all, field) /= RESULT_OK then
                     rc := RESULT_FATAL;
                  end if;

               when vital =>
                  if pkg_boolean (pkg_access, cur.all, field) /= RESULT_OK then
                     rc := RESULT_FATAL;
                  end if;

               when messages =>
                  if pkg_message (pkg_access, cur.all, field) /= RESULT_OK then
                     rc := RESULT_FATAL;
                  end if;

            end case;
         end;
      end loop;

      return rc;
   end pkg_parse_manifest_ucl;


   --------------------------------------------------------------------
   --  get_field
   --------------------------------------------------------------------
   function get_field (key : String) return pkg_field
   is
      total_keywords : constant := pkg_field'Pos (pkg_field'Last) + 1;

      subtype keyword_string is String (1 .. 15);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : pkg_field;
         end record;

      --  Keep in alphabetical order (critical!)
      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("NOTFOUND       ", NOTFOUND),
         ("abi            ", abi),
         ("annotations    ", annotations),
         ("arch           ", arch),
         ("categories     ", categories),
         ("comment        ", comment),
         ("config         ", pkg_config_files),
         ("conflicts      ", pkg_conflicts),
         ("deps           ", deps),
         ("desc           ", description),
         ("directories    ", directories),
         ("dirs           ", pkg_dirs),
         ("files          ", files),
         ("flatsize       ", flatsize),
         ("groups         ", pkg_groups),
         ("licenselogic   ", liclogic),
         ("licenses       ", licenses),
         ("maintainer     ", maintainer),
         ("messages       ", messages),
         ("name           ", name),
         ("options        ", options),
         ("origin         ", origin),
         ("path           ", path),
         ("pkgsize        ", pkgsize),
         ("prefix         ", prefix),
         ("provides       ", pkg_provides),
         ("repopath       ", repopath),
         ("requires       ", pkg_requires),
         ("scripts        ", scripts),
         ("shlibs_provided", pkg_shlibs_prov),
         ("shlibs_required", pkg_shlibs_reqd),
         ("sum            ", checksum),
         ("users          ", pkg_users),
         ("version        ", version),
         ("vital          ", vital),
         ("www            ", www)
        );

      bandolier : keyword_string := (others => ' ');
      Low       : Natural := all_keywords'First;
      High      : Natural := all_keywords'Last;
      Mid       : Natural;
   begin
      if key'Length > keyword_string'Length or else
        key'Length < 2
      then
         return NOTFOUND;
      end if;

      bandolier (1 .. key'Length) := key;

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
   end get_field;

end Core.Manifest;
