--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Interfaces.C.Extensions;
private with System;

with Libfetch;

package Core.Repo.SSH is

   bad_http_index : exception;
   bad_srv_index  : exception;

   type Mirror_Host is
      record
         scheme : Text;
         host   : Text;
         port   : Natural;
         doc    : Text;
      end record;

   type SRV_Host is
      record
         host   : Text;
         port   : Natural;
      end record;

   function start_ssh
     (my_repo        : Repo_Cursor;
      url_components : Libfetch.URL_Component_Set;
      size           : out int64) return Action_Result;

   --  Sets SRV mirrors from the given url
   --  If none found, zero is returned -- otherwise 1 is returned
   function set_http_mirrors
     (my_repo   : Repo_Cursor;
      url       : String) return Natural;

   function get_http_mirror
     (my_repo  : Repo_Cursor;
      index    : Natural) return Mirror_Host;

   function get_srv_information
     (my_repo  : Repo_Cursor;
      index    : Natural) return SRV_Host;

   function total_http_mirrors (my_repo : Repo_Cursor) return Natural;
   function total_srv_records (my_repo : Repo_Cursor) return Natural;

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
     (my_repo        : Repo_Cursor;
      url_components : Libfetch.URL_Component_Set) return String;

   function convert_to_mirror (url : String) return A_http_mirror;

   procedure close_ssh (Key : text; Element : in out A_repo);
   procedure fork_ssh  (Key : text; Element : in out A_repo);

end Core.Repo.SSH;
