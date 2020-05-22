--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Unix;
with libucl;

package Core.Repo.Meta is

   function meta_load
     (fd      : Unix.File_Descriptor;
      success : out Action_Result) return Repo_Metadata;

private

   invalid_meta_version : constant A_Meta_Version := 0;

   function meta_version (top : access libucl.ucl_object_t) return A_Meta_Version;

   function meta_open_schema_v1 return access libucl.ucl_object_t;

   function meta_parse
     (top     : access libucl.ucl_object_t;
      version : A_Meta_Version) return Repo_Metadata;

   function meta_set_default return Repo_Metadata;

   function meta_parse_cert
     (obj : access constant libucl.ucl_object_t) return Meta_Certificate;

end Core.Repo.Meta;
