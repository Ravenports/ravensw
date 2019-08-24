--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Deps is

   type pkg_dep_version_op is
     (VERSION_ANY,
      VERSION_EQ,
      VERSION_GE,
      VERSION_LE,
      VERSION_LT,
      VERSION_GT,
      VERSION_NOT);

   function pkg_deps_string_toop (instr : String) return pkg_dep_version_op;

end Core.Deps;
