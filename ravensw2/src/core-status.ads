--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Status is

   type Pkg_Status is
     (PKG_STATUS_NODB,
      PKG_STATUS_BAD_DB,
      PKG_STATUS_NOPACKAGES,
      PKG_STATUS_ACTIVE);

   type Pkg_Status_Output is
      record
         status : Pkg_Status;
         count  : Integer;
      end record;

   --  Used for activation status check
   function ravensw_status return Pkg_Status_Output;

end Core.Status;
