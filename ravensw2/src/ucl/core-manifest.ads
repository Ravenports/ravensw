--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Pkgtypes;
private with libucl;
private with Ucl;

package Core.Manifest is

   function parse_manifest
     (pkg_access : Pkgtypes.A_Package_Access;
      manifest   : String) return Action_Result;

private

   type pkg_field is
     (NOTFOUND,
      abi,
      annotations,
      arch,
      categories,
      checksum,
      comment,
      deps,
      description,
      directories,
      files,
      flatsize,
      licenses,
      liclogic,
      maintainer,
      messages,
      name,
      options,
      origin,
      pkg_config_files,
      pkg_conflicts,
      pkg_dirs,
      pkg_groups,
      pkg_provides,
      pkg_requires,
      pkg_shlibs_prov,
      pkg_shlibs_reqd,
      pkg_users,
      pkgsize,
      prefix,
      repopath,
      scripts,
      version,
      vital,
      www
     );

   function pkg_string
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result;

   function pkg_obj
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result;

   function pkg_array
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result;

   function pkg_int
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result;

   function pkg_boolean
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result;

   function pkg_message
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Action_Result;

   function url_decode (original : String) return String;

   function pkg_set_dirs_from_object
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : access constant libucl.ucl_object_t) return Action_Result;

   function pkg_set_files_from_object
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : access constant libucl.ucl_object_t) return Action_Result;

   function pkg_set_deps_from_object
     (pkg_access : Pkgtypes.A_Package_Access;
      obj   : access constant libucl.ucl_object_t) return Action_Result;

   function script_type
     (scrtype_string : String;
      valid : out Boolean) return Pkgtypes.Package_Script;

   function pkg_parse_manifest_ucl
     (pkg_access : Pkgtypes.A_Package_Access;
      obj : access constant libucl.ucl_object_t) return Action_Result;

   function get_field (key : String) return pkg_field;

end Core.Manifest;
