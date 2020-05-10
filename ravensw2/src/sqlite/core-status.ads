--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Status is

   type Activation_Status is
     (ACT_STATUS_NODB,
      ACT_STATUS_BAD_DB,
      ACT_STATUS_NOPACKAGES,
      ACT_STATUS_ACTIVE);

   type Activation_Status_Output is
      record
         status : Activation_Status;
         count  : Integer;
      end record;

   --  Used for activation status check
   function ravensw_status return Activation_Status_Output;

end Core.Status;
