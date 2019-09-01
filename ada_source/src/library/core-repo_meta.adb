--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Core.Strings; use Core.Strings;
with Core.Event;
with Core.Checksum;
with Ucl;
with System;

package body Core.Repo_Meta is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  pkg_repo_meta_load
   --------------------------------------------------------------------
   function pkg_repo_meta_load (fd : Unix.File_Descriptor; success : out Pkg_Error_Type)
                                return T_pkg_repo_meta
   is
      parser  : Ucl.T_parser;
      top     : access libucl.ucl_object_t;
      version : T_meta_version;
      result  : T_pkg_repo_meta := pkg_repo_meta_set_default;
   begin
      success := EPKG_FATAL;
      parser := Ucl.ucl_parser_new_lowercase;

      if not Ucl.ucl_parser_add_fd (parser, fd) then
         Event.pkg_emit_error
           (SUS ("cannot parse repository meta: " & Ucl.ucl_parser_get_error (parser)));
         libucl.ucl_parser_free (parser);
         return result;
      end if;

      top := Ucl.ucl_parser_get_object (parser);
      libucl.ucl_parser_free (parser);

      version := pkg_repo_meta_version (top);
      case version is
         when invalid_meta_version =>
            Event.pkg_emit_error (SUS ("repository meta has wrong version or wrong format"));
            libucl.ucl_object_unref (top);
            return result;
         when 1 =>
            declare
               schema : access libucl.ucl_object_t := pkg_repo_meta_open_schema_v1;
               errmsg : Text;
            begin
               if schema = null then
                  Event.pkg_emit_error (SUS ("internal error, pkg_repo_meta_load, schema=null"));
                  libucl.ucl_object_unref (top);
                  return result;
               else
                  if not Ucl.ucl_object_valid_per_schema (schema, top, errmsg) then
                     Event.pkg_emit_error
                       (SUS ("repository meta cannot be validated: " & USS (errmsg)));
                     libucl.ucl_object_unref (schema);
                     libucl.ucl_object_unref (top);
                     return result;
                  end if;
               end if;

               libucl.ucl_object_unref (schema);
            end;
      end case;

      result := pkg_repo_meta_parse (top, version);
      libucl.ucl_object_unref (top);
      success := EPKG_OK;

      return result;

   end pkg_repo_meta_load;


   --------------------------------------------------------------------
   --  pkg_repo_meta_version
   --------------------------------------------------------------------
   function pkg_repo_meta_version (top : access libucl.ucl_object_t) return T_meta_version
   is
      use type Ucl.int64;
      obj : access constant libucl.ucl_object_t;
      raw : Ucl.int64;
   begin
      obj := Ucl.ucl_object_find_key (top, "version");
      if obj /= null then
         if Ucl.type_is_integer (obj) then
            raw := Ucl.ucl_object_toint (obj);
            if raw > 0 and then raw <= Ucl.int64 (T_meta_version'Last) then
               return T_meta_version (raw);
            end if;
         end if;
      end if;

      return invalid_meta_version;
   end pkg_repo_meta_version;


   --------------------------------------------------------------------
   --  pkg_repo_meta_open_schema_v1
   --------------------------------------------------------------------
   function pkg_repo_meta_open_schema_v1 return access libucl.ucl_object_t
   is
      parser  : Ucl.T_parser;
      result  : access libucl.ucl_object_t;
      meta_schema_str_v1 : constant String :=
        "{" &
        "type = object;" &
        "properties {" &
        "version = {type = integer};" & LAT.LF &
        "maintainer = {type = string};" & LAT.LF &
        "source = {type = string};" & LAT.LF &
        "packing_format = {enum = [tzst, txz, tbz, tgz, tar]};" & LAT.LF &
        "digest_format = {enum = [sha256_base32, sha256_hex, blake2_base32, blake2s_base32]};" &
        LAT.LF &
        "digests = {type = string};" & LAT.LF &
        "manifests = {type = string};" & LAT.LF &
        "conflicts = {type = string};" & LAT.LF &
        "fulldb = {type = string};" & LAT.LF &
        "filesite = {type = string};" & LAT.LF &
        "digests_archive = {type = string};" & LAT.LF &
        "manifests_archive = {type = string};" & LAT.LF &
        "conflicts_archive = {type = string};" & LAT.LF &
        "fulldb_archive = {type = string};" & LAT.LF &
        "filesite_archive = {type = string};" & LAT.LF &
        "source_identifier = {type = string};" & LAT.LF &
        "revision = {type = integer};" & LAT.LF &
        "eol = {type = integer};" & LAT.LF &
        "cert = {" &
        "  type = object;" & LAT.LF &
        "  properties {" &
        "    type = {enum = [rsa]};" & LAT.LF &
        "    data = {type = string};" & LAT.LF &
        "    name = {type = string};" & LAT.LF &
        "  }" &
        "  required = [type, data, name];" & LAT.LF &
        "};" & LAT.LF
        &
        "}" & LAT.LF &
        "required = [version]" & LAT.LF &
        "}";
   begin
      parser := Ucl.ucl_parser_new_nofilevars;
      if not Ucl.ucl_parser_add_chunk (parser, meta_schema_str_v1) then
         Event.pkg_emit_error
           (SUS ("cannot parse schema for repo meta: " & Ucl.ucl_parser_get_error (parser)));
         libucl.ucl_parser_free (parser);
         return null;
      end if;

      result := Ucl.ucl_parser_get_object (parser);
      libucl.ucl_parser_free (parser);
      return result;
   end pkg_repo_meta_open_schema_v1;


   --------------------------------------------------------------------
   --  pkg_repo_meta_parse
   --------------------------------------------------------------------
   function pkg_repo_meta_parse
     (top     : access libucl.ucl_object_t;
      version : T_meta_version) return T_pkg_repo_meta
   is
      function meta_extract_string (field : String) return Text;
      function meta_extract_string (field : String) return Text
      is
         obj : access constant libucl.ucl_object_t;
      begin
         obj := Ucl.ucl_object_find_key (top, field);
         if obj /= null and then
           Ucl.type_is_string (obj)
         then
            return SUS (Ucl.ucl_object_tostring (obj));
         else
            return blank;
         end if;
      end meta_extract_string;

      result : T_pkg_repo_meta := pkg_repo_meta_set_default;
   begin
      result.version           := version;
      result.packing_format    := TZS;   -- hardcoded (not selectable anymore)
      result.maintainer        := meta_extract_string ("maintainer");
      result.source            := meta_extract_string ("source");
      result.source_identifier := meta_extract_string ("source_identifier");
      result.conflicts         := meta_extract_string ("conflicts");
      result.conflicts_archive := meta_extract_string ("conflicts_archive");
      result.digests           := meta_extract_string ("digests");
      result.digests_archive   := meta_extract_string ("digests_archive");
      result.manifests         := meta_extract_string ("manifests");
      result.manifests_archive := meta_extract_string ("manifests_archive");
      result.fulldb            := meta_extract_string ("fulldb");
      result.fulldb_archive    := meta_extract_string ("fulldb_archive");
      result.filesite          := meta_extract_string ("filesite");
      result.filesite_archive  := meta_extract_string ("filesite_archive");
      result.digest_format     := Checksum.pkg_checksum_type_from_string
                                  (USS (meta_extract_string ("digest_format")));

      declare
         obj : access constant libucl.ucl_object_t;
      begin
         obj := Ucl.ucl_object_find_key (top, "eol");
         if obj /= null and then
           Ucl.type_is_integer (obj)
         then
            result.end_of_life := T_EOL (Ucl.ucl_object_toint (obj));
         end if;
      end;

      declare
         obj : access constant libucl.ucl_object_t;
      begin
         obj := Ucl.ucl_object_find_key (top, "revision");
         if obj /= null and then
           Ucl.type_is_integer (obj)
         then
            result.revision := Integer (Ucl.ucl_object_toint (obj));
         end if;
      end;

      declare
         obj  : access constant libucl.ucl_object_t;
         item : access constant libucl.ucl_object_t;
         iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      begin
         obj := Ucl.ucl_object_find_key (top, "cert");
         loop
            item := Ucl.ucl_object_iterate (obj, iter'Access, True);
            exit when item = null;

            result.cert.Append (pkg_repo_meta_parse_cert (item));
         end loop;
      end;

      return result;
   end pkg_repo_meta_parse;


   --------------------------------------------------------------------
   --  pkg_repo_meta_set_default
   --------------------------------------------------------------------
   function pkg_repo_meta_set_default return T_pkg_repo_meta
   is
      meta : T_pkg_repo_meta;
   begin
      meta.version := T_meta_version'Last;
      meta.digest_format := PKG_HASH_TYPE_SHA256_BASE32;
      meta.packing_format := TZS;

      meta.manifests := SUS ("packagesite.yaml");
      meta.manifests_archive := SUS ("packagesite");
      meta.digests := SUS ("digests");
      meta.digests_archive := SUS ("digests");
      meta.filesite := SUS ("filesite.yaml");
      meta.filesite_archive := SUS ("filesite");

      return meta;
   end pkg_repo_meta_set_default;


   --------------------------------------------------------------------
   --  pkg_repo_meta_parse_cert
   --------------------------------------------------------------------
   function pkg_repo_meta_parse_cert (obj : access constant libucl.ucl_object_t)
                                      return T_pkg_repo_meta_cert
   is
      result : T_pkg_repo_meta_cert;
   begin
      result.name        := SUS (Ucl.ucl_object_tostring (Ucl.ucl_object_find_key (obj, "name")));
      result.pubkey      := SUS (Ucl.ucl_object_tostring (Ucl.ucl_object_find_key (obj, "data")));
      --  result.pubkey_type :=
      --              SUS (Ucl.ucl_object_tostring (Ucl.ucl_object_find_key (obj, "type")));

      --  There's only one supported type, hardcode instead of reading.
      result.pubkey_type := rsa;
      return result;
   end pkg_repo_meta_parse_cert;

end Core.Repo_Meta;
