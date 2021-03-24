--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Latin_1;
with Ada.Calendar;
with System;

with Core.Event;
with Core.Config;
with Core.Utilities;

with Resolve;
with fetch_h;

package body Core.Repo.SSH is

   package LAT renames Ada.Characters.Latin_1;
   package CAL renames Ada.Calendar;

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
      --  We're only using 1 iovec structure, so we're limited to IC.size_t number of
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
      check_timeout : Boolean := False;
      deadline      : CAL.Time;
      current_time  : CAL.Time;
      fds_ready     : Integer;
      timeout_ms    : Integer;
   begin
      iov.iov_len  := buflen;
      iov.iov_base := message (index)'Unchecked_Access;

      if Libfetch.get_fetch_timeout > 0 then
         check_timeout := True;
         deadline := CAL."+" (CAL.Clock, Duration (Libfetch.get_fetch_timeout));
      end if;

      Event.emit_debug (1, "writing data");
      loop
         exit when done;
         if check_timeout then
            current_time := CAL.Clock;
            if CAL.">" (current_time, deadline) then
               Unix.set_ETIMEDOUT;
               return -1;
            end if;
            declare
               diff_seconds : CAL.Day_Duration;
            begin
               diff_seconds := CAL."-" (deadline, current_time);
               timeout_ms := Integer (1000.0 * diff_seconds);
            end;
            Unix.reset_errno;
            loop
               fds_ready := Unix.poll_write (my_repo.ssh_io.fd_out, timeout_ms);
               exit when fds_ready /= -1;
               if not Unix.last_error_INTR then
                  return -1;
               end if;
            end loop;
         end if;
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


   --------------------------------------------------------------------
   --  ssh_read
   --------------------------------------------------------------------
   function ssh_read
     (data   : System.Address;
      buffer : System.Address;
      buflen : IC.size_t) return IC.Extensions.long_long
   is
      use type IC.size_t;

      my_repo : A_repo;
      for my_repo'Address use data;

      message : IC.char_array (1 .. buflen);
      for message'Address use buffer;

      rlen          : int64 := -1;
      index         : IC.size_t := message'First;
      check_timeout : Boolean := False;
      deadline      : CAL.Time;
      current_time  : CAL.Time;
      onward        : Boolean;
      fds_ready     : Integer;
      timeout_ms    : Integer := -1;
   begin
      Event.emit_debug (2, "ssh: start reading");

      if Libfetch.get_fetch_timeout > 0 then
         check_timeout := True;
         deadline := CAL."+" (CAL.Clock, Duration (Libfetch.get_fetch_timeout));
      end if;

      loop
         onward := True;
         declare
            data_from_socket : String := Unix.read_fd (my_repo.ssh_io.fd_in, Natural (buflen));
         begin
            if IsBlank (data_from_socket) then
               if Unix.last_error_INTR then
                  onward := False;
               else
                  if not Unix.last_error_AGAIN then
                     Event.emit_errno ("ssh_read", "timeout", Unix.errno);
                     return -1;
                  end if;
               end if;
            else
               rlen := data_from_socket'Length;
               for x in data_from_socket'Range loop
                  message (index) := IC.char'Val (Character'Pos (data_from_socket (x)));
               end loop;
               exit;
            end if;
         end;

         if onward then
            --  only EAGAIN should get here
            if check_timeout then
               current_time := CAL.Clock;
               if CAL.">" (current_time, deadline) then
                  Unix.set_ETIMEDOUT;
                  return -1;
               end if;
               declare
                  diff_seconds : CAL.Day_Duration;
               begin
                  diff_seconds := CAL."-" (deadline, current_time);
                  timeout_ms := Integer (1000.0 * diff_seconds);
               end;
            end if;

            Unix.reset_errno;
            Event.emit_debug (1, "ssh_read: begin poll()");
            fds_ready := Unix.poll_read (my_repo.ssh_io.fd_in, timeout_ms);
            if fds_ready < 0 then
               if Unix.last_error_INTR then
                  onward := False;
                  Event.emit_debug (1, "ssh_read: poll interrupted");
               else
                  Event.emit_debug (1, "ssh_read: poll failed");
                  return -1;
               end if;
            end if;
            if onward then
               Event.emit_debug (1, "ssh_read: end poll()");
            end if;
         end if;
      end loop;

      return IC.Extensions.long_long (rlen);
   end ssh_read;


   --------------------------------------------------------------------
   --  start_ssh
   --------------------------------------------------------------------
   function start_ssh
     (my_repo        : Repo_Cursor;
      url_components : Libfetch.URL_Component_Set;
      size           : out int64) return Action_Result
   is
      ssh_cookie_functions : fetch_h.es_cookie_io_functions_t;
      R : A_repo renames Repository_Crate.Element (my_repo.position);
   begin
      ssh_cookie_functions.func_read  := ssh_read'Access;
      ssh_cookie_functions.func_write := ssh_write'Access;
      ssh_cookie_functions.func_close := ssh_close'Access;

      if not Libfetch.stream_is_active (R.ssh) then
         declare
            procedure open_ssh (Key : text; Element : in out A_repo);

            use type Unix.Process_ID;
            use type Unix.File_Descriptor;

            sshin   : aliased Unix.Two_Sockets;
            sshout  : aliased Unix.Two_Sockets;
            ssh_cmd : constant String := compose_ssh_command (my_repo, url_components);

            --------------------------------------------------------------------
            --  open_ssh
            --------------------------------------------------------------------
            procedure open_ssh (Key : text; Element : in out A_repo) is
            begin
               Element.ssh_io.fd_in := sshout (0);
               Element.ssh_io.fd_out := sshin (1);
               Unix.set_blocking (Element.ssh_io.fd_in);

               Element.ssh := Libfetch.open_cookie (cookie    => Element'Address,
                                                    functions => ssh_cookie_functions);
            end open_ssh;
         begin
            if not Unix.socket_pair_stream (sshin'Access) or else
              not Unix.socket_pair_stream (sshout'Access)
            then
               return RESULT_FATAL;
            end if;
            repositories.Update_Element (my_repo.position, fork_ssh'Access);
            if R.ssh_io.pid < 0 then
               Event.emit_errno ("start_ssh", "fork", Unix.errno);
               return RESULT_FATAL;
            end if;

            if R.ssh_io.pid = 0 then
               --  child process

               if not
                 (Unix.dup2 (sshin (0), Unix.stdin_file_fd) and then
                  Unix.close_file (sshin (1)) and then
                  Unix.close_file (sshout (0)) and then
                  Unix.dup2 (sshout (1), Unix.stdout_file_fd))
               then
                  Event.emit_errno ("start_ssh", "prepare_pipes", Unix.errno);
                  return RESULT_FATAL;
               end if;
               if sshin (0) /= Unix.stdin_file_fd then
                  Event.emit_error ("start_ssh: Unexpected value for child's STDIN");
                  if not Unix.close_file (sshin (0)) then
                     Event.emit_error ("start_ssh: Failed to close child's STDIN");
                  end if;
               end if;
               if sshout (1) /= Unix.stdout_file_fd then
                  Event.emit_error ("start_ssh: Unexpected value for child's STDOUT");
                  if not Unix.close_file (sshout (1)) then
                     Event.emit_error ("start_ssh: Failed to close child's STDOUT");
                  end if;
               end if;

               Event.emit_debug (1, "Fetch: running " & SQ (ssh_cmd));
               Unix.execvp (Utilities.bourne_shell, "-c" & LAT.LF & ssh_cmd);
               --  Beyond here is never reached (end of child's life)
            end if;

            --  Parent Process
            if not Unix.close_file (sshout (1)) or else
              not Unix.close_file (sshin (0))
            then
               Event.emit_errno ("start_ssh", "Failed to close pipe", Unix.errno);
               return RESULT_FATAL;
            end if;
            Event.emit_debug (1, "SSH> connected");

            repositories.Update_Element (my_repo.position, open_ssh'Access);

            if not Libfetch.stream_is_active (R.ssh) then
               Event.emit_errno ("start_ssh", "Failed to close stream", Unix.errno);
               return RESULT_FATAL;
            end if;

            declare
               done : Boolean;
               line : constant String := Libfetch.fx_getline (R.ssh, done);
            begin
               if done then
                  Event.emit_debug (1, "SSH> nothing to read, got: " & line);
                  repositories.Update_Element (my_repo.position, close_ssh'Access);
                  return RESULT_OK;
               else
                  if leads (line, "ok:") then
                     Event.emit_debug
                       (1, "SSH> server is: " & line (line'First + 3  .. line'Last));
                  else
                     Event.emit_debug (1, "SSH> server rejected, got: " & line);
                     repositories.Update_Element (my_repo.position, close_ssh'Access);
                     return RESULT_OK;
                  end if;
               end if;
            end;
         end;
      end if;   --  END activate repo's SSH block

      declare
         info : constant String := "get "
           & Libfetch.url_doc (url_components)
           & Libfetch.url_ims_time (url_components)'Img;
      begin
         Event.emit_debug (1, "SSH> " & info);
         Libfetch.fx_print (R.ssh, info);
      end;
      declare
         done : Boolean;
         line : constant String := Libfetch.fx_getline (R.ssh, done);
      begin
         if not done then
            Event.emit_debug (1, "SSH> recv: " & line);
            if leads (line, "ok:") then
               begin
                  size := int64'Value (line (line'First + 3 .. line'Last));
               exception
                  when others =>
                     Event.emit_error ("start_ssh: failed to parse " & line);
                     repositories.Update_Element (my_repo.position, close_ssh'Access);
                     return RESULT_FATAL;
               end;
               if size = 0 then
                  repositories.Update_Element (my_repo.position, close_ssh'Access);
                  return RESULT_UPTODATE;
               end if;
            end if;
         end if;
      end;
      repositories.Update_Element (my_repo.position, close_ssh'Access);
      return RESULT_OK;
   end start_ssh;


   --------------------------------------------------------------------
   --  compose_ssh_command
   --------------------------------------------------------------------
   function compose_ssh_command
     (my_repo        : Repo_Cursor;
      url_components : Libfetch.URL_Component_Set) return String
   is
      cmd_text : Text;
      ssh_args : constant String  := Config.configuration_value (Config.ssh_args);
      user     : constant String  := Libfetch.url_user (url_components);
      host     : constant String  := Libfetch.url_host (url_components);
      port     : constant Natural := Libfetch.url_port (url_components);
      R        : A_repo renames Repository_Crate.Element (my_repo.position);
   begin
      cmd_text := SUS ("/usr/bin/ssh -e none -T");
      if not IsBlank (ssh_args) then
         SU.Append (cmd_text, " " & ssh_args);
      end if;
      case R.flags is
         when Repo.REPO_FLAGS_LIMIT_IPV4 =>
            SU.Append (cmd_text, " -4");
         when Repo.REPO_FLAGS_LIMIT_IPV6 =>
            SU.Append (cmd_text, " -6");
         when Repo.REPO_FLAGS_DEFAULT =>
            null;
      end case;
      if port > 0 then
         SU.Append (cmd_text, " -p " & int2str (port));
      end if;
      if not IsBlank (user) then
         SU.Append (cmd_text, " " & user & "@");
      end if;
      SU.Append (cmd_text, host);
      SU.Append (cmd_text, " " & progname & " ssh");
      return USS (cmd_text);
   end compose_ssh_command;


   --------------------------------------------------------------------
   --  set_http_mirrors
   --------------------------------------------------------------------
   function set_http_mirrors
     (my_repo   : Repo_Cursor;
      url       : String) return Natural
   is
      fstream : Libfetch.Fetch_Stream;
      index   : Natural := 0;
      R       : A_repo renames Repository_Crate.Element (my_repo.position);
   begin
      if not R.http.Is_Empty then
         return 1;
      end if;

      fstream := Libfetch.fx_GetURL (url, "");
      if not Libfetch.stream_is_active (fstream) then
         return 0;
      end if;

      loop
         declare
            done : Boolean;
            line : constant String := Libfetch.fx_getline (R.ssh, done);
         begin
            exit when done;
            if leads (line, "URL:") then
               declare
                  procedure add_mirror (Key : text; Element : in out A_repo);

                  trimmed : String := trim (line (line'First + 4 .. line'Last));

                  procedure add_mirror (Key : text; Element : in out A_repo) is
                  begin
                     Element.http.Append  (convert_to_mirror (trimmed));
                  end add_mirror;
               begin
                  if not IsBlank (trimmed) then
                     repositories.Update_Element (my_repo.position, add_mirror'Access);
                     index := 1;
                  end if;
               end;
            end if;
         end;
      end loop;
      Libfetch.fx_close (fstream);
      return index;

   end set_http_mirrors;


   --------------------------------------------------------------------
   --  set_http_mirrors
   --------------------------------------------------------------------
   function convert_to_mirror (url : String) return A_http_mirror
   is
      components : Libfetch.URL_Component_Set;
      result     : A_http_mirror;
   begin
      components      := Libfetch.parse_url (url);
      result.scheme   := SUS (Libfetch.url_scheme   (components));
      result.user     := SUS (Libfetch.url_user     (components));
      result.pwd      := SUS (Libfetch.url_password (components));
      result.host     := SUS (Libfetch.url_host     (components));
      result.doc      := SUS (Libfetch.url_doc      (components));
      result.port     := Libfetch.url_port     (components);
      result.offset   := Libfetch.url_offset   (components);
      result.length   := Libfetch.url_length   (components);
      result.ims_time := Libfetch.url_ims_time (components);
      result.netrcfd  := Libfetch.url_netrcfd  (components);
      return result;
   end convert_to_mirror;


   --------------------------------------------------------------------
   --  get_http_mirror
   --------------------------------------------------------------------
   function get_http_mirror
     (my_repo  : Repo_Cursor;
      index    : Natural) return Mirror_Host
   is
      procedure flip (position : A_http_mirror_crate.Cursor);

      result : Mirror_Host;
      track  : Natural := 0;

      procedure flip (position : A_http_mirror_crate.Cursor)
      is
         site : A_http_mirror renames A_http_mirror_crate.Element (position);
      begin
         track := track + 1;
         if track = index then
            result.host   := site.host;
            result.port   := site.port;
            result.scheme := site.scheme;
            result.doc    := site.doc;
         end if;
      end flip;
   begin
      if index = 0 or else index > Natural (total_http_mirrors (my_repo)) then
         raise bad_http_index;
      end if;
      Repository_Crate.Element (my_repo.position).http.Iterate (flip'Access);
      return result;
   end get_http_mirror;


   --------------------------------------------------------------------
   --  get_srv_information
   --------------------------------------------------------------------
   function get_srv_information
     (my_repo  : Repo_Cursor;
      index    : Natural) return SRV_Host
   is
      procedure flip (position : Resolve.Answer_Crate.Cursor);

      result : SRV_Host;
      track  : Natural := 0;

      procedure flip (position : Resolve.Answer_Crate.Cursor)
      is
         site : Resolve.An_Answer renames Resolve.Answer_Crate.Element (position);
      begin
         track := track + 1;
         if track = index then
            result.host   := site.target;
            result.port   := Natural (site.port);
         end if;
      end flip;
   begin
      if index = 0 or else index > Natural (total_srv_records (my_repo)) then
         raise bad_srv_index;
      end if;
      Repository_Crate.Element (my_repo.position).srv.Iterate (flip'Access);
      return result;
   end get_srv_information;


   --------------------------------------------------------------------
   --  total_http_mirrors
   --------------------------------------------------------------------
   function total_http_mirrors (my_repo : Repo_Cursor) return Natural is
   begin
      return Natural (Repository_Crate.Element (my_repo.position).http.Length);
   end total_http_mirrors;


   --------------------------------------------------------------------
   --  total_srv_records
   --------------------------------------------------------------------
   function total_srv_records (my_repo : Repo_Cursor) return Natural is
   begin
      return Natural (Repository_Crate.Element (my_repo.position).srv.Length);
   end total_srv_records;


   --------------------------------------------------------------------
   --  close_ssh
   --------------------------------------------------------------------
   procedure close_ssh (Key : text; Element : in out A_repo) is
   begin
      Libfetch.fx_close (Element.ssh);
   end close_ssh;


   --------------------------------------------------------------------
   --  fork_ssh
   --------------------------------------------------------------------
   procedure fork_ssh (Key : text; Element : in out A_repo) is
   begin
      Element.ssh_io.pid := Unix.fork;
   end fork_ssh;


end Core.Repo.SSH;
