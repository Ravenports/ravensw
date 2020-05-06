--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

-- with Core.Metalog;
with Core.Context;
with Core.Unix;

package body Core.Finalize is

   --------------------------------------------------------------------
   --  cleanup
   --------------------------------------------------------------------
   procedure cleanup is
   begin
      -- Metalog.metalog_close;
      Context.close_eventpipe;
      Context.close_root_fd;

   end cleanup;

end Core.Finalize;
