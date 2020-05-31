--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Interfaces.C.Extensions;
private with System;

with Libfetch;

package Core.Repo.SSH is

   function start_ssh
     (my_repo        : in out A_repo;
      url_components : Libfetch.URL_Component_Set;
      size           : out int64) return Action_Result;

private

   package IC renames Interfaces.C;

   function ssh_close (data : System.Address) return IC.int;
   pragma Convention (C, ssh_close);

   function ssh_write
     (data   : System.Address;
      buffer : System.Address;
      buflen : IC.size_t) return IC.Extensions.long_long;
   pragma Convention (C, ssh_write);

   function ssh_read
     (data   : System.Address;
      buffer : System.Address;
      buflen : IC.size_t) return IC.Extensions.long_long;
   pragma Convention (C, ssh_read);

   function compose_ssh_command
     (my_repo        : in out A_repo;
      url_components : Libfetch.URL_Component_Set) return String;

end Core.Repo.SSH;
