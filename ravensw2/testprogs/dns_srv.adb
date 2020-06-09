--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Resolve;

procedure DNS_SRV is

   package TIO renames Ada.Text_IO;

   zone1 : constant String := "_sip._udp.7006581001.siptrunking.appiaservices.com";
   zone2 : constant String := "_sip._udp.srv.lcr.thinq.com";

begin

   declare
      payload : constant String := Resolve.resolve_query (zone1);
   begin
      if payload'Length > 12 then
         declare
            response : Resolve.DNS_Response := Resolve.translate_response (payload);
         begin
            Resolve.sort_response (response);
            Resolve.dump_response (response);
         end;
      else
         TIO.Put_Line ("Error: Query response is shorter than the header requirements.");
      end if;
   end;

   declare
      payload : constant String := Resolve.resolve_query (zone2);
   begin
      if payload'Length > 12 then
         declare
            response : Resolve.DNS_Response := Resolve.translate_response (payload);
         begin
            Resolve.sort_response (response);
            Resolve.dump_response (response);
         end;
      else
         TIO.Put_Line ("Error: Query response is shorter than the header requirements.");
      end if;
   end;

end DNS_SRV;
