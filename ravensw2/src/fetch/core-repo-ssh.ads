--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Interfaces.C.Extensions;
private with System;

with Libfetch;

package Core.Repo.SSH is

   function start_ssh
     (my_repo        : A_repo;
      url_components : Libfetch.URL_Component_Set;
      size           : out int64) return Action_Result;

private

   package IC renames Interfaces.C;

   function ssh_close (data : System.Address) return IC.int;

   function ssh_write
     (data   : System.Address;
      buffer : System.Address;
      buflen : IC.size_t) return IC.Extensions.long_long;

   function ssh_read
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : IC.size_t) return IC.Extensions.long_long;

end Core.Repo.SSH;
