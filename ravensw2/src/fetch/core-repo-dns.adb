--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body Core.Repo.DNS is

   --------------------------------------------------------------------
   --  set_dns_srvinfo
   --------------------------------------------------------------------
   procedure set_dns_srvinfo
     (my_repo   : in out A_repo;
      zone      : String)
   is
   begin
      if not my_repo.srv.Is_Empty then
         return;
      end if;


   end set_dns_srvinfo;

end Core.Repo.DNS;
