--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;  use Core.Pkg;
private with libucl;
private with Ucl;

package Core.Manifest is

   function pkg_parse_manifest
     (pkg_access : T_pkg_Access;
      manifest   : String) return Pkg_Error_Type;

private

   type pkg_field is
     (NOTFOUND,
      abi,
      annotations,
      arch,
      categories,
      checksum,
      comment,
      depend_formula,
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
     (pkg_access : T_pkg_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Pkg_Error_Type;

   function pkg_obj
     (pkg_access : T_pkg_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Pkg_Error_Type;

   function pkg_array
     (pkg_access : T_pkg_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Pkg_Error_Type;

   function pkg_int
     (pkg_access : T_pkg_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Pkg_Error_Type;

   function pkg_boolean
     (pkg_access : T_pkg_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Pkg_Error_Type;

   function pkg_message
     (pkg_access : T_pkg_Access;
      obj   : aliased libucl.ucl_object_t;
      field : pkg_field) return Pkg_Error_Type;

   function url_decode (original : String) return String;

   function pkg_set_dirs_from_object
     (pkg_access : T_pkg_Access;
      obj   : access constant libucl.ucl_object_t) return Pkg_Error_Type;

   function pkg_set_files_from_object
     (pkg_access : T_pkg_Access;
      obj   : access constant libucl.ucl_object_t) return Pkg_Error_Type;

   function pkg_set_deps_from_object
     (pkg_access : T_pkg_Access;
      obj   : access constant libucl.ucl_object_t) return Pkg_Error_Type;

   function script_type
     (scrtype_string : String;
      valid : out Boolean) return pkg_script_type;

   function pkg_parse_manifest_ucl
     (pkg_access : T_pkg_Access;
      obj : access constant libucl.ucl_object_t) return Pkg_Error_Type;

   function get_field (key : String) return pkg_field;

end Core.Manifest;
