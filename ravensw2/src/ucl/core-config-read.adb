--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Latin_1;

with Core.Strings;
with Core.Context;
with Core.Object;
with Ucl;


package body Core.Config.Read is

   package LAT renames Ada.Characters.Latin_1;
   package COB renames Core.Object;
   package CS  renames Core.Strings;


   --------------------------------------------------------------------
   --  config_get
   --------------------------------------------------------------------
   function config_get (key : String) return access constant libucl.ucl_object_t is
   begin
      return Ucl.ucl_object_find_key (config_object, key);
   end config_get;


   --------------------------------------------------------------------
   --  config_get_string
   --------------------------------------------------------------------
   function config_get_string (key : String) return String is
   begin
      return COB.pkg_object_string (config_get (key));
   end config_get_string;


   --------------------------------------------------------------------
   --  config_get_boolean
   --------------------------------------------------------------------
   function config_get_boolean (key : String) return Boolean is
   begin
      return COB.pkg_object_bool (config_get (key));
   end config_get_boolean;


   --------------------------------------------------------------------
   --  config_get_int64
   --------------------------------------------------------------------
   function config_get_int64 (key : String) return int64 is
   begin
      return COB.pkg_object_int (config_get (key));
   end config_get_int64;


   --------------------------------------------------------------------
   --  config_dump
   --------------------------------------------------------------------
   function config_dump return String is
   begin
      return Object.pkg_object_dump (config_object);
   end config_dump;


   --------------------------------------------------------------------
   --  ucl_configuration_value #1 (Boolean result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return Boolean is
   begin
      if config_get_type (ci) = pkg_bool then
         return config_get_boolean (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


   --------------------------------------------------------------------
   --  ucl_configuration_value #2 (int64 result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return int64 is
   begin
      if config_get_type (ci) = pkg_int then
         return config_get_int64 (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


   --------------------------------------------------------------------
   --  ucl_configuration_value #3 (string result)
   --------------------------------------------------------------------
   function ucl_configuration_value (ci : Configuration_Item) return String is
   begin
      if config_get_type (ci) = pkg_string then
         return config_get_string (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end ucl_configuration_value;


   --------------------------------------------------------------------
   --  convert
   --------------------------------------------------------------------
   function convert (obj : access constant libucl.ucl_object_t) return Config_Entry_Type
   is
      ut : libucl.ucl_type_t := libucl.ucl_object_type (obj);
   begin
      case ut is
         when libucl.UCL_OBJECT  => return pkg_object;
         when libucl.UCL_ARRAY   => return pkg_array;
         when libucl.UCL_INT     => return pkg_int;
         when libucl.UCL_STRING  => return pkg_string;
         when libucl.UCL_BOOLEAN => return pkg_bool;
         when libucl.UCL_FLOAT |
              libucl.UCL_TIME |
              libucl.UCL_USERDATA |
              libucl.UCL_NULL =>
            raise Unsupported_Type with ut'Img;
      end case;
   end convert;


   --------------------------------------------------------------------
   --  convert_string_to_ucl
   --------------------------------------------------------------------
   function convert_string_to_ucl_object (cetype  : Config_Entry_Type;
                                          payload : String) return access libucl.ucl_object_t
   is
      obj          : access libucl.ucl_object_t;
      inserted     : Boolean;
      rootdir_used : constant Boolean := not IsBlank (Context.reveal_pkg_rootdir);
   begin
      case cetype is
         when pkg_string =>
            declare
               tmp     : Text;
               tmp_set : Boolean := False;
            begin
               if rootdir_used and then leads (payload, "/")
               then
                  SU.Append (tmp, Context.reveal_pkg_rootdir);
                  SU.Append (tmp, payload);
                  tmp_set := True;
               end if;
               if tmp_set then
                  obj := Ucl.ucl_object_fromstring_and_trim (USS (tmp));
               else
                  obj := Ucl.ucl_object_fromstring_and_trim (payload);
               end if;
            end;
         when pkg_bool =>
            obj := Ucl.ucl_object_fromstring_boolean (payload);
         when pkg_int =>
            obj := Ucl.ucl_object_fromstring_int (payload);
         when pkg_array =>
            obj := Ucl.ucl_object_typed_new_array;
            --  format A,B,C,D
            if not IsBlank (payload) then
               declare
                  nf  : Natural := CS.count_char (payload, LAT.Comma);
               begin
                  for k in 1 .. nf + 1 loop
                     inserted := Ucl.ucl_array_push (obj,
                                                     Ucl.ucl_object_fromstring_and_trim
                                                       (CS.specific_field (payload, k, ",")));
                  end loop;
               end;
            end if;
         when pkg_object =>
            obj := Ucl.ucl_object_typed_new_object;
            --  format A=B,C=D,E=F,G=H
            if not IsBlank (payload) then
               declare
                  nf  : Natural := CS.count_char (payload, LAT.Comma);
               begin
                  for k in 1 .. nf + 1 loop
                     declare
                        nvpair : constant String := CS.specific_field (payload, k, ",");
                     begin
                        if CS.contains (nvpair, "=") then
                           inserted := Ucl.ucl_object_insert_key
                             (top      => obj,
                              elt      =>
                                Ucl.ucl_object_fromstring_and_trim (CS.part_2 (nvpair)),
                              key      => CS.part_1 (nvpair),
                              copy_key => False);
                        end if;
                     end;
                  end loop;
               end;
            end if;
      end case;
      return obj;
   end convert_string_to_ucl_object;


end Core.Config.Read;
