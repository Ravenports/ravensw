--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Core.Repo.DNS is

   --  parses zone to produce one or more dns servers
   --  If at least one produced, return 1, otherwise return 0
   function set_dns_srvinfo
     (my_repo   : in out A_repo;
      zone      : String) return Natural;

end Core.Repo.DNS;
