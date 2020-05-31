--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Latin_1;
with Core.Event;

package body Core.Repo.SSH is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  ssh_close
   --------------------------------------------------------------------
   function ssh_close (data : System.Address) return IC.int
   is
      my_repo : A_repo;
      for my_repo'Address use data;
      exit_status : Integer;
   begin
      if not Unix.write_to_file_descriptor (my_repo.ssh_io.fd_out, "quit" & LAT.LF) then
         Event.emit_notice ("ssh_close: Failed to sent quit message");
      end if;

      if not Unix.wait_for_pid (my_repo.ssh_io.pid, exit_status) then
         return 1;  --  equivalent of RESULT_FATAL
      end if;

      Libfetch.fx_close (my_repo.ssh);
      return IC.int (exit_status);
   end ssh_close;


   --------------------------------------------------------------------
   --  ssh_write
   --------------------------------------------------------------------
   function ssh_write
     (data   : System.Address;
      buffer : System.Address;
      buflen : IC.size_t) return IC.Extensions.long_long
   is
      --  We're only only 1 iovec structure, so we're limited to IC.size_t number of
      --  characters for the message (32 bits version 64 bit result)
      use type IC.size_t;

      my_repo : A_repo;
      for my_repo'Address use data;

      message : IC.char_array (1 .. buflen);
      for message'Address use buffer;

      wlen   : int64;
      total  : int64 := 0;
      done   : Boolean := False;
      iov    : aliased Unix.iovec;
      index  : IC.size_t := message'First;
   begin
      iov.iov_len  := buflen;
      iov.iov_base := message (index)'Unchecked_Access;

      Event.emit_debug (1, "writing data");
      loop
         exit when done;
         Unix.reset_errno;
         wlen := Unix.sendmsg (my_repo.ssh_io.fd_out, iov'Access, 0);
         if wlen = 0 then
            Unix.set_ECONNRESET;
            return -1;
         elsif wlen < 0 then
            return -1;
         end if;
         total := total + wlen;
         if wlen >= int64 (buflen) then
            done := True;
         end if;
         if not done then
            index := index + IC.size_t (wlen);
            iov.iov_len := iov.iov_len - IC.size_t (wlen);
            iov.iov_base := message (index)'Unchecked_Access;
         end if;
      end loop;
      return IC.Extensions.long_long (total);
   end ssh_write;

end Core.Repo.SSH;
