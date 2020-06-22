--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Resolve;

package body Core.Repo.DNS is

   --------------------------------------------------------------------
   --  set_dns_srvinfo
   --------------------------------------------------------------------
   function set_dns_srvinfo
     (my_repo : Repo_Cursor;
      zone    : String) return Natural
   is
      R : A_repo renames Repository_Crate.Element (my_repo.position);
   begin
      if not R.srv.Is_Empty then
         return 0;
      end if;

      declare
         header_and_payload : constant String := Resolve.resolve_query (zone);
      begin
         if header_and_payload'Length < 12 then
            return 0;
         end if;

         declare
            procedure attach_answers (Key : text; Element : in out A_repo);

            response : Resolve.DNS_Response := Resolve.translate_response (header_and_payload);

            procedure attach_answers (Key : text; Element : in out A_repo) is
            begin
               Element.srv := response.answers;
            end attach_answers;
         begin
            if not response.valid then
               return 0;
            end if;

            Resolve.sort_response (response);
            repositories.Update_Element (my_repo.position, attach_answers'Access);
            if R.srv.Is_Empty then
               return 0;
            else
               return 1;
            end if;
         end;
      end;
   end set_dns_srvinfo;

end Core.Repo.DNS;
