--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Containers.Vectors;
with Core;

private with Ada.Numerics.Discrete_Random;
private with resolv_h;

use Core;

package Resolve is

   package CON renames Ada.Containers;

   type Generic16 is mod 2 ** 16;
   type Generic32 is mod 2 ** 32;
   type Normalized_Weight is range 0 .. 100;

   type QType_Value is
     (T_A,     T_NS,    T_MD,    T_MF,
      T_CNAME, T_SOA,   T_MB,    T_MG,
      T_MR,    T_NULL,  T_WKS,   T_PTR,
      T_HINFO, T_MINFO, T_MX,    T_TXT,
      T_SRV,   T_AXFR,  T_MAILB, T_MAILA,
      T_ANY,   T_WRONG);

   type AType_Value is
     (T_A,     T_NS,    T_MD,    T_MF,
      T_CNAME, T_SOA,   T_MB,    T_MG,
      T_MR,    T_NULL,  T_WKS,   T_PTR,
      T_HINFO, T_MINFO, T_MX,    T_TXT,
      T_SRV,   T_WRONG);

   type QClass_Value is
     (C_IN, C_CS, C_CH, C_HS, C_ANY, C_WRONG);

   type AClass_Value is
     (C_IN, C_CS, C_CH, C_HS, C_WRONG);

   type A_Question is
      record
         question : Text;
         qtype    : QType_Value;
         qclass   : QClass_Value;
      end record;

   type An_Answer is
      record
         name     : Text;
         atype    : AType_Value;
         aclass   : AClass_Value;
         ttl      : Generic32;
         rdlength : Generic16;
         priority : Generic16;
         weight   : Generic16;
         port     : Generic16;
         target   : Text;
         weight2  : Normalized_Weight;
      end record;

   package Question_Crate is new CON.Vectors
     (Element_Type => A_Question,
      Index_Type   => Natural);

   package Answer_Crate is new CON.Vectors
     (Element_Type => An_Answer,
      Index_Type   => Natural);

   type DNS_Response is
      record
         questions : Question_Crate.Vector;
         answers   : Answer_Crate.Vector;
         valid     : Boolean;
      end record;

   function resolve_query (domain_name : String) return String;
   function translate_response (response : String) return DNS_Response;
   procedure sort_response (response : in out DNS_Response);

   procedure dump_response (response : DNS_Response);

private

   subtype DNS_Header is String (1 .. 12);
   type An_Opcode is mod 2 ** 4;

   --  12 bytes
   type Arpa_Header is
      record
         id      : Generic16;  --  query identification number
                               --  3rd byte (little endian)
         rd      : Boolean;    --  recursion desired
         tc      : Boolean;    --  truncated message
         aa      : Boolean;    --  authoritive answer
         opcode  : An_Opcode;  --  purpose of message
         qr      : Boolean;    --  response flag
                               --  4th byte (little endian)
         rcode   : An_Opcode;  --  response flag
         cd      : Boolean;    --  checking disabled by resolver
         ad      : Boolean;    --  authentic data from named
         zero    : Boolean;    --  unused bits (MBZ as of 4.9.3a3)
         ra      : Boolean;    --  recursion available
                               --  remaining bytes
         qdcount : Generic16;  --  number of question entries
         ancount : Generic16;  --  number of answer entries
	 nscount : Generic16;  --  number of authority entries
         arcount : Generic16;  --  number of resource entries
      end record;
   subtype bytes2 is String (1 .. 2);
   subtype bytes4 is String (1 .. 4);

   --  Convert binary string of 2 characters in Network Byte Order (Big Endian) to 16-bit integer
   function nbo2int (str : bytes2) return Generic16;

   --  Convert binary string of 4 characters in Network Byte Order to 32-bit integer
   function nbo2int (str : bytes4) return Generic32;

   function ns_class_equivalent (raw : Generic16) return resolv_h.ns_class;
   function ns_type_equivalent (raw : Generic16) return resolv_h.ns_type;
   function conv_qclass (qclass : resolv_h.ns_class) return QClass_Value;
   function conv_aclass (aclass : resolv_h.ns_class) return AClass_Value;
   function conv_qtype  (qtype  : resolv_h.ns_type) return QType_Value;
   function conv_atype  (atype  : resolv_h.ns_type) return AType_Value;

   function convert_header (response : DNS_Header) return Arpa_Header;

   function host_less_than (A, B : An_Answer) return Boolean;

   package Host_Sorter is new Answer_Crate.Generic_Sorting ("<" => host_less_than);
   package Rand_Weight is new Ada.Numerics.Discrete_Random (Normalized_Weight);

end Resolve;
