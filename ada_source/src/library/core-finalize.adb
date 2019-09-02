--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Metalog;
with Core.Pkg;   use Core.Pkg;
with Core.Unix;

package body Core.Finalize is

   --------------------------------------------------------------------
   --  cleanup
   --------------------------------------------------------------------
   procedure cleanup
   is
      successful_close : Boolean;
   begin
      Metalog.metalog_close;
      successful_close := Unix.close_file (context.eventpipe);
      successful_close := Unix.close_file (context.rootfd);

   end cleanup;

end Core.Finalize;
