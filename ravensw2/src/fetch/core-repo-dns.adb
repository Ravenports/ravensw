--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Resolve;

package body Core.Repo.DNS is

   --------------------------------------------------------------------
   --  set_dns_srvinfo
   --------------------------------------------------------------------
   function set_dns_srvinfo
     (my_repo   : in out A_repo;
      zone      : String) return Natural
   is
   begin
      if not my_repo.srv.Is_Empty then
         return 0;
      end if;

      declare
         header_and_payload : constant String := Resolve.resolve_query (zone);
      begin
         if header_and_payload'Length < 12 then
            return 0;
         end if;

         declare
            response : Resolve.DNS_Response := Resolve.translate_response (header_and_payload);
         begin
            if not response.valid then
               return 0;
            end if;

            Resolve.sort_response (response);
            my_repo.srv := response.answers;
            if my_repo.srv.Is_Empty then
               return 0;
            else
               return 1;
            end if;
         end;
      end;
   end set_dns_srvinfo;

end Core.Repo.DNS;
