--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Interfaces.C.Strings;
with Ada.Text_IO;
with System;

with Core.Strings;

package body Resolve is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
   package INT renames Interfaces;
   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------
   --  resolve_query
   --------------------------------------------------------------------
   function resolve_query (zone : String) return String
   is
      use type IC.int;

      res    : IC.int;
      dname  : ICS.chars_ptr;
      anslen : IC.int := IC.int (1024);
      answer : aliased IC.char_array (1 .. IC.size_t (anslen));
   begin
      dname := ICS.New_String (zone);
      res := resolv_h.C_res_query (dname  => dname,
                                   rclass => resolv_h.ns_c_in,
                                   rtype  => resolv_h.ns_t_srv,
                                   answer => answer (answer'First)'Access,
                                   anslen => anslen);
      ICS.Free (dname);
      if res <= 0 then
         return "";
      else
         declare
            response_len : constant Positive := Positive (res);
            response     : String (1 .. response_len);
         begin
            for x in 1 .. Positive (res) loop
               response (x) := Character'Val (IC.char'Pos (answer (IC.size_t (x))));
            end loop;
            return response;
         end;
      end if;
   end resolve_query;


   --------------------------------------------------------------------
   --  nbo2int #1
   --------------------------------------------------------------------
   function nbo2int (str : bytes2) return Generic16
   is
      use type INT.Unsigned_16;
      --  Input is in big endian order.  [A][B] => 0x0A0B

      A : INT.Unsigned_16 := INT.Unsigned_16 (Character'Pos (str (str'First)));
      B : INT.Unsigned_16 := INT.Unsigned_16 (Character'Pos (str (str'First + 1)));
   begin
      return Generic16 (INT.Shift_Left (A, 8) + B);
   end nbo2int;


   --------------------------------------------------------------------
   --  nbo2int #2
   --------------------------------------------------------------------
   function nbo2int (str : bytes4) return Generic32
   is
      use type INT.Unsigned_32;
      --  Input is in big endian order.  [A][B][C][D] => 0x0A0B0C0D

      A : INT.Unsigned_32 := INT.Unsigned_32 (Character'Pos (str (str'First)));
      B : INT.Unsigned_32 := INT.Unsigned_32 (Character'Pos (str (str'First + 1)));
      C : INT.Unsigned_32 := INT.Unsigned_32 (Character'Pos (str (str'First + 2)));
      D : INT.Unsigned_32 := INT.Unsigned_32 (Character'Pos (str (str'First + 3)));
   begin
      return Generic32
        (INT.Shift_Left (A, 24) + INT.Shift_Left (B, 16) + INT.Shift_Left (C, 8) + D);
   end nbo2int;


   --------------------------------------------------------------------
   --  convert_header
   --------------------------------------------------------------------
   function convert_header (response : DNS_Header) return Arpa_Header
   is
      use type INT.Unsigned_8;

      function convert_2_flag (arg : INT.Unsigned_8) return Boolean;
      function convert_2_flag (arg : INT.Unsigned_8) return Boolean is
      begin
         return (arg /= 0);
      end convert_2_flag;

      answer : Arpa_Header;

      M2 : INT.Unsigned_8 := INT.Unsigned_8 (Character'Pos (response (response'First + 2)));
      M3 : INT.Unsigned_8 := INT.Unsigned_8 (Character'Pos (response (response'First + 3)));
   begin
      answer.id      := nbo2int (response (response'First .. response'First + 1));
      answer.qdcount := nbo2int (response (response'First + 4  .. response'First + 5));
      answer.ancount := nbo2int (response (response'First + 6  .. response'First + 7));
      answer.nscount := nbo2int (response (response'First + 8  .. response'First + 9));
      answer.arcount := nbo2int (response (response'First + 10 .. response'First + 11));

      answer.qr      := convert_2_flag (M2 and 2#1000_0000#);
      answer.opcode  := An_Opcode (INT.Shift_Right (M2 and 2#0111_1000#, 3));
      answer.aa      := convert_2_flag (M2 and 2#0000_0100#);
      answer.tc      := convert_2_flag (M2 and 2#0000_0010#);
      answer.rd      := convert_2_flag (M2 and 2#0000_0001#);

      answer.ra      := convert_2_flag (M3 and 2#1000_0000#);
      answer.zero    := convert_2_flag (M3 and 2#0100_0000#);
      answer.ad      := convert_2_flag (M3 and 2#0010_0000#);
      answer.cd      := convert_2_flag (M3 and 2#0001_0000#);
      answer.rcode   := An_Opcode (M3 and 2#0000_1111#);

      return answer;
   end convert_header;


   --------------------------------------------------------------------
   --  conv_qclass
   --------------------------------------------------------------------
   function conv_qclass (qclass : resolv_h.ns_class) return QClass_Value is
   begin
      case qclass is
         when resolv_h.ns_c_in    => return C_IN;
         when resolv_h.ns_c_2     => return C_CS;
         when resolv_h.ns_c_chaos => return C_CH;
         when resolv_h.ns_c_hs    => return C_HS;
         when resolv_h.ns_c_any   => return C_ANY;
         when others              => return C_WRONG;
      end case;
   end conv_qclass;


   --------------------------------------------------------------------
   --  conv_aclass
   --------------------------------------------------------------------
   function conv_aclass (aclass : resolv_h.ns_class) return AClass_Value is
   begin
      case aclass is
         when resolv_h.ns_c_in    => return C_IN;
         when resolv_h.ns_c_2     => return C_CS;
         when resolv_h.ns_c_chaos => return C_CH;
         when resolv_h.ns_c_hs    => return C_HS;
         when others              => return C_WRONG;
      end case;
   end conv_aclass;


   --------------------------------------------------------------------
   --  conv_qtype
   --------------------------------------------------------------------
   function conv_qtype (qtype : resolv_h.ns_type) return QType_Value is
   begin
      case qtype is
         when resolv_h.ns_t_a     => return T_A;
         when resolv_h.ns_t_ns    => return T_NS;
         when resolv_h.ns_t_md    => return T_MD;
         when resolv_h.ns_t_mf    => return T_MF;
         when resolv_h.ns_t_cname => return T_CNAME;
         when resolv_h.ns_t_soa   => return T_SOA;
         when resolv_h.ns_t_mb    => return T_MB;
         when resolv_h.ns_t_mg    => return T_MG;
         when resolv_h.ns_t_mr    => return T_MR;
         when resolv_h.ns_t_null  => return T_NULL;
         when resolv_h.ns_t_wks   => return T_WKS;
         when resolv_h.ns_t_ptr   => return T_PTR;
         when resolv_h.ns_t_hinfo => return T_HINFO;
         when resolv_h.ns_t_minfo => return T_MINFO;
         when resolv_h.ns_t_mx    => return T_MX;
         when resolv_h.ns_t_txt   => return T_TXT;
         when resolv_h.ns_t_srv   => return T_SRV;
         when resolv_h.ns_t_axfr  => return T_AXFR;
         when resolv_h.ns_t_mailb => return T_MAILB;
         when resolv_h.ns_t_maila => return T_MAILA;
         when resolv_h.ns_t_any   => return T_ANY;
         when others              => return T_WRONG;
      end case;
   end conv_qtype;


   --------------------------------------------------------------------
   --  conv_atype
   --------------------------------------------------------------------
   function conv_atype (atype : resolv_h.ns_type) return AType_Value
   is
   begin
      case atype is
         when resolv_h.ns_t_a     => return T_A;
         when resolv_h.ns_t_ns    => return T_NS;
         when resolv_h.ns_t_md    => return T_MD;
         when resolv_h.ns_t_mf    => return T_MF;
         when resolv_h.ns_t_cname => return T_CNAME;
         when resolv_h.ns_t_soa   => return T_SOA;
         when resolv_h.ns_t_mb    => return T_MB;
         when resolv_h.ns_t_mg    => return T_MG;
         when resolv_h.ns_t_mr    => return T_MR;
         when resolv_h.ns_t_null  => return T_NULL;
         when resolv_h.ns_t_wks   => return T_WKS;
         when resolv_h.ns_t_ptr   => return T_PTR;
         when resolv_h.ns_t_hinfo => return T_HINFO;
         when resolv_h.ns_t_minfo => return T_MINFO;
         when resolv_h.ns_t_mx    => return T_MX;
         when resolv_h.ns_t_txt   => return T_TXT;
         when resolv_h.ns_t_srv   => return T_SRV;
         when others              => return T_WRONG;
      end case;
   end conv_atype;


   --------------------------------------------------------------------
   --  ns_class_equivalent
   --------------------------------------------------------------------
   function ns_class_equivalent (raw : Generic16) return resolv_h.ns_class is
   begin
      case raw is
         when      1 => return resolv_h.ns_c_in;
         when      2 => return resolv_h.ns_c_2;
         when      3 => return resolv_h.ns_c_chaos;
         when      4 => return resolv_h.ns_c_hs;
         when    254 => return resolv_h.ns_c_none;
         when    255 => return resolv_h.ns_c_any;
         when others => return resolv_h.ns_c_invalid;
      end case;
   end ns_class_equivalent;


   --------------------------------------------------------------------
   --  ns_type_equivalent
   --------------------------------------------------------------------
   function ns_type_equivalent (raw : Generic16) return resolv_h.ns_type is
   begin
      case raw is
         when      1 => return resolv_h.ns_t_a;
         when      2 => return resolv_h.ns_t_ns;
         when      3 => return resolv_h.ns_t_md;
         when      4 => return resolv_h.ns_t_mf;
         when      5 => return resolv_h.ns_t_cname;
         when      6 => return resolv_h.ns_t_soa;
         when      7 => return resolv_h.ns_t_mb;
         when      8 => return resolv_h.ns_t_mg;
         when      9 => return resolv_h.ns_t_mr;
         when     10 => return resolv_h.ns_t_null;
         when     11 => return resolv_h.ns_t_wks;
         when     12 => return resolv_h.ns_t_ptr;
         when     13 => return resolv_h.ns_t_hinfo;
         when     14 => return resolv_h.ns_t_minfo;
         when     15 => return resolv_h.ns_t_mx;
         when     16 => return resolv_h.ns_t_txt;
         when     17 => return resolv_h.ns_t_rp;
         when     18 => return resolv_h.ns_t_afsdb;
         when     19 => return resolv_h.ns_t_x25;
         when     20 => return resolv_h.ns_t_isdn;
         when     21 => return resolv_h.ns_t_rt;
         when     22 => return resolv_h.ns_t_nsap;
         when     23 => return resolv_h.ns_t_nsap_ptr;
         when     24 => return resolv_h.ns_t_sig;
         when     25 => return resolv_h.ns_t_key;
         when     26 => return resolv_h.ns_t_px;
         when     27 => return resolv_h.ns_t_gpos;
         when     28 => return resolv_h.ns_t_aaaa;
         when     29 => return resolv_h.ns_t_loc;
         when     30 => return resolv_h.ns_t_nxt;
         when     31 => return resolv_h.ns_t_eid;
         when     32 => return resolv_h.ns_t_nimloc;
         when     33 => return resolv_h.ns_t_srv;
         when     34 => return resolv_h.ns_t_atma;
         when     35 => return resolv_h.ns_t_naptr;
         when     36 => return resolv_h.ns_t_kx;
         when     37 => return resolv_h.ns_t_cert;
         when     38 => return resolv_h.ns_t_a6;
         when     39 => return resolv_h.ns_t_dname;
         when     40 => return resolv_h.ns_t_sink;
         when     41 => return resolv_h.ns_t_opt;
         when     42 => return resolv_h.ns_t_apl;
         when     43 => return resolv_h.ns_t_ds;
         when     44 => return resolv_h.ns_t_sshfp;
         when     45 => return resolv_h.ns_t_ipseckey;
         when     46 => return resolv_h.ns_t_rrsig;
         when     47 => return resolv_h.ns_t_nsec;
         when     48 => return resolv_h.ns_t_dnskey;
         when     49 => return resolv_h.ns_t_dhcid;
         when     50 => return resolv_h.ns_t_nsec3;
         when     51 => return resolv_h.ns_t_nsec3param;
         when     55 => return resolv_h.ns_t_hip;
         when     99 => return resolv_h.ns_t_spf;
         when    249 => return resolv_h.ns_t_tkey;
         when    250 => return resolv_h.ns_t_tsig;
         when    251 => return resolv_h.ns_t_ixfr;
         when    252 => return resolv_h.ns_t_axfr;
         when    253 => return resolv_h.ns_t_mailb;
         when    254 => return resolv_h.ns_t_maila;
         when    255 => return resolv_h.ns_t_any;
         when    256 => return resolv_h.ns_t_zxfr;
         when  32769 => return resolv_h.ns_t_dlv;
         when others => return resolv_h.ns_t_invalid;
      end case;
   end ns_type_equivalent;


   --------------------------------------------------------------------
   --  translate_response
   --------------------------------------------------------------------
   function translate_response (response : String) return DNS_Response
   is
      use type IC.size_t;

      function slice2 (bindex : IC.size_t) return bytes2;
      function slice4 (bindex : IC.size_t) return bytes4;

      hostlen  : constant IC.size_t := 1024;
      host     : aliased IC.char_array (1 .. hostlen);
      buffer   : aliased IC.char_array (1 .. response'Length);
      index    : IC.size_t := buffer'First;
      eomindex : IC.size_t := buffer'Last;
      result   : DNS_Response;
      header   : Arpa_Header;

      function slice2 (bindex : IC.size_t) return bytes2 is
      begin
         return IC.To_Ada (buffer (bindex)) & IC.To_Ada (buffer (bindex + 1));
      end slice2;

      function slice4 (bindex : IC.size_t) return bytes4 is
      begin
         return IC.To_Ada (buffer (bindex))
           & IC.To_Ada (buffer (bindex + 1))
           & IC.To_Ada (buffer (bindex + 2))
           & IC.To_Ada (buffer (bindex + 3));
      end slice4;

   begin
      result.valid := False;
      if response'Length < 12 then
         return result;
      end if;
      header := convert_header (response (response'First .. response'First + 11));

      --  convert response back to char array
      for x in response'Range loop
         buffer (index) := IC.char'Val (Character'Pos (response (x)));
         index := index + 1;
      end loop;

      index := buffer'First + 12;

      declare
         qdcount : Natural := Natural (header.qdcount);
      begin
         loop
            exit when qdcount = 0;
            exit when index > eomindex;  -- shouldn't happen
            qdcount := qdcount - 1;
            declare
               len       : IC.int;
               frag      : A_Question;
               raw_type  : Generic16;
               raw_class : Generic16;
            begin
               len := resolv_h.C_dn_expand (msg     => buffer (buffer'First)'Access,
                                            eomorig => buffer (buffer'Last)'Access,
                                            comp_dn => buffer (index)'Access,
                                            exp_dn  => host (host'First)'Access,
                                            length  => IC.int (hostlen));
               index := index + IC.size_t (len);
               raw_type  := nbo2int (slice2 (index));
               raw_class := nbo2int (slice2 (index + 2));
               index := index + 4;

               frag.question := Strings.SUS (IC.To_Ada (host));
               frag.qtype    := conv_qtype  (ns_type_equivalent (raw_type));
               frag.qclass   := conv_qclass (ns_class_equivalent (raw_class));
               result.questions.Append (frag);
            end;
         end loop;
      end;

      declare
         ancount : Natural := Natural (header.ancount);
      begin
         loop
            exit when ancount = 0;
            exit when index > eomindex;  -- shouldn't happen
            ancount := ancount - 1;
            declare
               len       : IC.int;
               answer    : An_Answer;
               raw_type  : Generic16;
               raw_class : Generic16;
            begin
               len := resolv_h.C_dn_expand (msg     => buffer (buffer'First)'Access,
                                            eomorig => buffer (buffer'Last)'Access,
                                            comp_dn => buffer (index)'Access,
                                            exp_dn  => host (host'First)'Access,
                                            length  => IC.int (hostlen));
               index := index + IC.size_t (len);
               raw_type  := nbo2int (slice2 (index));
               raw_class := nbo2int (slice2 (index + 2));
               index := index + 4;
               answer.name     := Strings.SUS (IC.To_Ada (host));
               answer.atype    := conv_atype  (ns_type_equivalent (raw_type));
               answer.aclass   := conv_aclass (ns_class_equivalent (raw_class));
               answer.ttl      := nbo2int (slice4 (index));
               answer.rdlength := nbo2int (slice2 (index + 4));
               index := index + 6;
               case answer.atype is
                  when T_SRV =>
                     answer.priority := nbo2int (slice2 (index));
                     answer.weight   := nbo2int (slice2 (index + 2));
                     answer.port     := nbo2int (slice2 (index + 4));
                     index := index + 6;
                     len := resolv_h.C_dn_expand (msg     => buffer (buffer'First)'Access,
                                                  eomorig => buffer (buffer'Last)'Access,
                                                  comp_dn => buffer (index)'Access,
                                                  exp_dn  => host (host'First)'Access,
                                                  length  => IC.int (hostlen));
                     answer.target := Strings.SUS (IC.To_Ada (host));
                     index := index + IC.size_t (len);
                  when others =>
                     answer.priority := 0;
                     answer.weight   := 0;
                     answer.port     := 0;
                     answer.target   := Strings.SUS ("not-a-srv-record!");
               end case;
               answer.weight2 := 0;
               result.answers.Append (answer);
            end;
         end loop;
      end;
      result.valid := True;
      return result;

   end translate_response;


   --------------------------------------------------------------------
   --  sort_response
   --------------------------------------------------------------------
   procedure sort_response (response : in out DNS_Response)
   is
      --  If all the priorities are unique, we ignore weights and just sort.
      --  If any priorities are shared, final weights need to be calculated for those
      --  before sorting.

      procedure update_weight2 (element : in out An_Answer);
      procedure adjust_weighting (canvas : String; num_duplicates : Natural);

      total : Natural := Natural (response.answers.Length);
      pass_weight2 : Normalized_Weight := 0;

      procedure update_weight2 (Element : in out An_Answer) is
      begin
         Element.weight2 := pass_weight2;
      end update_weight2;

      procedure adjust_weighting (canvas : String; num_duplicates : Natural)
      is
         combined : Generic16 := 0;
         facter   : Generic16;
         index : Natural;
         gen   : Rand_Weight.Generator;
      begin
         Rand_Weight.Reset (gen);
         for x in canvas'First .. canvas'First + num_duplicates - 1 loop
            index := Character'Pos (canvas (x)) - 1;
            combined := combined + response.answers.Element (index).weight;
         end loop;

         for x in canvas'First .. canvas'First + num_duplicates - 1 loop
            index  := Character'Pos (canvas (x)) - 1;
            facter := Generic16 (Rand_Weight.Random (gen));
            pass_weight2 :=
              Normalized_Weight (facter * response.answers.Element (index).weight / combined);
            response.answers.Update_Element (index, update_weight2'Access);
         end loop;
      end adjust_weighting;

   begin
      if total = 0 then
         return;
      end if;

      declare
         duplicates : String (1 .. total);
         exclude    : array (1 .. total) of Boolean := (others => False);
         num_found  : Natural;
      begin
         for x in 1 .. total - 1 loop
            num_found := 1;
            duplicates (num_found) := Character'Val (x);
            for y in x + 1 .. total loop
               if not exclude (x) and then not exclude (y) then
                  if response.answers.Element (x - 1).priority =
                    response.answers.Element (y - 1).priority
                  then
                     num_found := num_found + 1;
                     duplicates (num_found) := Character'Val (y);
                     exclude (y) := True;
                  end if;
               end if;
               if num_found > 1 then
                  adjust_weighting (duplicates, num_found);
               end if;
            end loop;
         end loop;
      end;

      --  Sort by priority, then normalized then randomly selected weight
      Host_Sorter.Sort (response.answers);

   end sort_response;


   --------------------------------------------------------------------
   --  host_less_than
   --------------------------------------------------------------------
   function host_less_than (A, B : An_Answer) return Boolean
   is
      --  sort priority lowest to highest, weights highest to lowest
   begin
      if A.priority = B.priority then
         return A.weight2 > B.weight2;
      else
         return A.priority < B.priority;
      end if;
   end host_less_than;


   --------------------------------------------------------------------
   --  dump_response
   --------------------------------------------------------------------
   procedure dump_response (response : DNS_Response)
   is
      procedure print_question (Position : Question_Crate.Cursor);
      procedure print_answer   (Position : Answer_Crate.Cursor);

      procedure print_question (Position : Question_Crate.Cursor)
      is
         question : A_Question renames Question_Crate.Element (position);
      begin
         TIO.Put_Line ("Question host : " & Strings.USS (question.question));
         TIO.Put_Line ("Question type : " & question.qtype'Img);
         TIO.Put_Line ("Question class: " & question.qclass'Img);
         TIO.Put_Line ("");
      end print_question;

      procedure print_answer (Position : Answer_Crate.Cursor)
      is
         answer : An_Answer renames Answer_Crate.Element (Position);
      begin
         TIO.Put_Line ("Answer name    : " & Strings.USS (answer.name));
         TIO.Put_Line ("Answer type    : " & answer.atype'Img);
         TIO.Put_Line ("Answer class   : " & answer.aclass'Img);
         TIO.Put_Line ("Answer TTL     :" & answer.ttl'Img);
         TIO.Put_Line ("answer RDLength:" & answer.rdlength'Img);
         TIO.Put_Line ("Answer priority:" & answer.priority'Img);
         TIO.Put_Line ("Answer weight  :" & answer.weight'Img);
         TIO.Put_Line ("Answer weight2 :" & answer.weight2'Img);
         TIO.Put_Line ("Answer target  : " & Strings.USS (answer.target));
         TIO.Put_Line ("Answer port    :" & answer.port'Img);
         TIO.Put_Line ("");
      end print_answer;
   begin
      if response.valid then
         TIO.Put_Line ("SRV records valid");
      else
         TIO.Put_Line ("### SRV records invalid ####");
      end if;
      response.questions.Iterate (print_question'Access);
      response.answers.Iterate (print_answer'Access);
   end dump_response;


end Resolve;
