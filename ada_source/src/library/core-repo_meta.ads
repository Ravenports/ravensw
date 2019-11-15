--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg; use Core.Pkg;
with Core.Unix;
with libucl;

package Core.Repo_Meta is

   function pkg_repo_meta_load (fd : Unix.File_Descriptor; success : out Pkg_Error_Type)
                                return T_pkg_repo_meta;

private

   function pkg_repo_meta_version (top : access libucl.ucl_object_t) return T_meta_version;

   function pkg_repo_meta_open_schema_v1 return access libucl.ucl_object_t;

   function pkg_repo_meta_parse
     (top     : access libucl.ucl_object_t;
      version : T_meta_version) return T_pkg_repo_meta;

   function pkg_repo_meta_set_default return T_pkg_repo_meta;

   function pkg_repo_meta_parse_cert (obj : access constant libucl.ucl_object_t)
                                      return T_pkg_repo_meta_cert;

end Core.Repo_Meta;
