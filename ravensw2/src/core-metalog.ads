--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Metalog is

   type pkg_metalog_type is (PKG_METALOG_FILE,
                             PKG_METALOG_DIR,
                             PKG_METALOG_LINK);

   function metalog_open (filename : String) return Action_Result;
   procedure metalog_add (mtype  : pkg_metalog_type;
                          path   : String;
                          uname  : String;
                          gname  : String;
                          link   : String;
                          mode   : Integer;
                          fflags : Natural);
   procedure metalog_close;

private

   function strtofflags_supported return Boolean;
   function strtoflags_plus_label (fflags : Natural) return String;

end Core.Metalog;
