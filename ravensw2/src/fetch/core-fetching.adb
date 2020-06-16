--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Environment_Variables;
with Interfaces.C;

with Core.Pkgtypes;
with Core.Strings;
with Core.Context;
with Core.Config;
with Core.Event;
with Core.Repo.SSH;
with Core.Repo.DNS;
with Libfetch;

use Core.Strings;

package body Core.Fetching is

   package ENV renames Ada.Environment_Variables;

   --------------------------------------------------------------------
   --  fetch_file_to_fd
   --------------------------------------------------------------------
   function fetch_file_to_fd
     (my_repo   : in out Repo.A_repo;
      file_url  : String;
      dest_fd   : Unix.File_Descriptor;
      timestamp : access Unix.T_epochtime;
      offset    : Unix.T_filesize;
      filesize  : Unix.T_filesize) return Action_Result
   is
      procedure set_env (position : Pkgtypes.Package_NVPairs.Cursor);
      procedure restore_env (position : Pkgtypes.Package_NVPairs.Cursor);

      max_retry     : constant int64 := Config.configuration_value (Config.fetch_retry);
      fetch_timeout : constant int64 := Config.configuration_value (Config.fetch_timeout);
      retry         : int64 := max_retry;

      URL_SCHEME_PREFIX : constant String := "pkg+";
      use_ssh           : Boolean;
      use_http          : Boolean;
      use_https         : Boolean;
      use_ftp           : Boolean;
      pkg_url_scheme    : Boolean;
      new_url           : Text;
      fetch_opts        : Text;
      size              : int64   := 0;
      http_index        : Natural := 0;
      srv_index         : Natural := 0;

      env_to_unset      : Pkgtypes.Text_Crate.Vector;
      env_to_restore    : Pkgtypes.Package_NVPairs.Map;

      url_components    : Libfetch.URL_Component_Set;
      remote            : Libfetch.Fetch_Stream;
      zerochar          : constant Interfaces.C.char := Interfaces.C.char'Val (0);
      docpath           : aliased Interfaces.C.char_array := (0 .. 1023 => zerochar);


      procedure set_env (position : Pkgtypes.Package_NVPairs.Cursor)
      is
         text_key : Text renames Pkgtypes.Package_NVPairs.Key (position);
         text_val : Text renames Pkgtypes.Package_NVPairs.Element (position);
         env_key  : String := USS (text_key);
         env_val  : String := USS (text_val);
      begin
         if ENV.Exists (env_key) then
            env_to_restore.Insert (text_key, SUS (ENV.Value (env_key)));
         else
            env_to_unset.Append (text_key);
         end if;
         env.Set (env_key, env_val);
      end set_env;

      procedure restore_env (position : Pkgtypes.Package_NVPairs.Cursor)
      is
         procedure unset (pos2 : Pkgtypes.Text_Crate.Cursor);
         procedure restore (pos2 : Pkgtypes.Package_NVPairs.Cursor);

         procedure unset (pos2 : Pkgtypes.Text_Crate.Cursor)
         is
            key : String := USS (Pkgtypes.Text_Crate.Element (pos2));
         begin
            ENV.Clear (key);
         end unset;

         procedure restore (pos2 : Pkgtypes.Package_NVPairs.Cursor)
         is
            env_key  : String := USS (Pkgtypes.Package_NVPairs.Key (pos2));
            env_val  : String := USS (Pkgtypes.Package_NVPairs.Element (pos2));
         begin
            ENV.Set (env_key, env_val);
         end restore;
      begin
         env_to_unset.iterate (unset'Access);
         env_to_restore.Iterate (restore'Access);
      end restore_env;

      use type Unix.T_filesize;
   begin
      Event.emit_debug (2, "fetch " & file_url & " for " & Repo.repo_name (my_repo));

      --  /* A URL of the form http://host.example.com/ where
      --   * host.example.com does not resolve as a simple A record is
      --   * not valid according to RFC 2616 Section 3.2.2.  Our usage
      --   * with SRV records is incorrect.  However it is encoded into
      --   * /usr/sbin/pkg in various releases so we can't just drop it.
      --   *
      --   * Instead, introduce new pkg+http://, pkg+https://,
      --   * pkg+ssh://, pkg+ftp://, pkg+file:// to support the
      --   * SRV-style server discovery, and also to allow eg. Firefox
      --   * to run pkg-related stuff given a pkg+foo:// URL.
      --   *
      --   * Error if using plain http://, https:// etc with SRV
      --   */

      if leads (file_url, URL_SCHEME_PREFIX) then
         case Repo.repo_mirror_type (my_repo) is
            when Repo.SRV => null;
            when Repo.HTTP | Repo.NOMIRROR =>
               Event.emit_error ("packagesite URL error for " & file_url
                                 & " -- " & URL_SCHEME_PREFIX
                                 &  ":// implies SRV mirror type");
               return RESULT_FATAL;
         end case;
         pkg_url_scheme := True;
         new_url := SUS (part_2 (file_url, URL_SCHEME_PREFIX));
      else
         pkg_url_scheme := False;
         new_url := SUS (file_url);
      end if;

      Repo.repo_environment (my_repo).Iterate (set_env'Access);

      url_components := Libfetch.parse_url (USS (new_url));
      if not Libfetch.url_is_valid (url_components) then
         Event.emit_error (USS (new_url) & ": parse error");
         Repo.repo_environment (my_repo).Iterate (restore_env'Access);
         return RESULT_FATAL;
      end if;

      if timestamp /= null then
         Libfetch.provide_IMS_timestamp (timestamp.all, url_components);
      end if;
      Libfetch.provide_offset (offset, url_components);

      declare
         scheme : constant String := Libfetch.url_scheme (url_components);
      begin
         use_ssh   := (scheme = "ssh");
         use_http  := (scheme = "http");
         use_https := (scheme = "https");
         use_ftp   := (scheme = "ftp");
      end;

      if use_ssh then
         if Repo.SSH.start_ssh (my_repo, url_components, size) /= RESULT_OK then
            Repo.repo_environment (my_repo).Iterate (restore_env'Access);
            Libfetch.free_url (url_components);
            return RESULT_FATAL;
         end if;
         remote := Repo.repo_ssh (my_repo);
      end if;

      loop
         exit when Libfetch.stream_is_active (remote);
         case Repo.repo_mirror_type (my_repo) is
            when Repo.SRV =>
               if use_http or else use_ftp then
                  declare
                     scheme : constant String := Libfetch.url_scheme (url_components);
                     host   : constant String := Libfetch.url_host (url_components);
                     zone   : constant String := "_" & scheme & "._tcp." & host;
                  begin
                     if not pkg_url_scheme then
                        Event.emit_notice
                          ("Warning: use of " & scheme
                           & ":// URL scheme with SRV records is deprecated: "
                           & "switch to pkg+" & scheme & "://");
                     end if;
                     srv_index := Repo.DNS.set_dns_srvinfo (my_repo, zone);
                  end;
               end if;
            when Repo.HTTP =>
               declare
                  function get_zone return String;
                  function get_zone return String
                  is
                     port : Natural := Libfetch.url_port (url_components);
                     host : constant String := Libfetch.url_host (url_components);
                  begin
                     if port = 0 then
                        if use_http then
                           port := 80;
                        else
                           port := 443;
                        end if;
                     end if;
                     if use_http then
                        return "http://" & host & ":" & int2str (port);
                     else
                        return "https://" & host & ":" & int2str (port);
                     end if;
                  end get_zone;
               begin
                  if use_http or else use_https then
                     http_index := Repo.SSH.set_http_mirrors (my_repo, get_zone);
                  end if;
               end;
            when Repo.NOMIRROR =>
               null;
         end case;

         case Repo.repo_mirror_type (my_repo) is
         when Repo.HTTP =>
            if http_index > 0 then
               declare
                  info : Repo.SSH.Mirror_Host;
                  doc_original : String := Libfetch.url_doc (url_components);
               begin
                  info := Repo.SSH.get_http_mirror (my_repo, http_index);
                  Libfetch.provide_host_information (USS (info.host), info.port, url_components);
                  Libfetch.provide_scheme (USS (info.scheme), url_components);
                  Libfetch.provide_doc (doc            => USS (info.doc) & doc_original,
                                        holder         => docpath'Unchecked_Access,
                                        url_components => url_components);
               end;
            end if;
         when Repo.SRV =>
            if srv_index > 0 then
               declare
                  info : Repo.SSH.SRV_Host;
               begin
                  info := Repo.SSH.get_srv_information (my_repo, srv_index);
                  Libfetch.provide_host_information (USS (info.host), info.port, url_components);
               end;
            end if;
         when Repo.NOMIRROR =>
            null;
         end case;

         fetch_opts := SUS ("i");
         case Repo.repo_ipv_type (my_repo) is
            when Repo.REPO_FLAGS_LIMIT_IPV4 =>
               SU.Append (fetch_opts, "4");
            when Repo.REPO_FLAGS_LIMIT_IPV6 =>
               SU.Append (fetch_opts, "6");
            when Repo.REPO_FLAGS_DEFAULT =>
               null;
         end case;
         if Context.reveal_debug_level = 4 then
            SU.Append (fetch_opts, "v");
         end if;

         Event.emit_debug
           (1, "Fetch: fetching from: "
            & Libfetch.url_scheme (url_components) & "://"
            & Libfetch.url_user_at_host (url_components)
            & Libfetch.url_doc (url_components)
            & " with opts " & DQ (USS (fetch_opts)));

         remote := Libfetch.fx_XGet (url_components, USS (fetch_opts));
         if not Libfetch.stream_is_active (remote) then
            if Libfetch.last_fetch_ok then
               Repo.repo_environment (my_repo).Iterate (restore_env'Access);
               Libfetch.free_url (url_components);
               return RESULT_UPTODATE;
            end if;

            retry := retry - 1;
            if retry <= 0 or else Libfetch.last_fetch_unavailable then
               Event.emit_error (file_url & ": " & Libfetch.get_last_fetch_error);
               Repo.repo_environment (my_repo).Iterate (restore_env'Access);
               Libfetch.free_url (url_components);
               return RESULT_FATAL;
            end if;

            case Repo.repo_mirror_type (my_repo) is
            when Repo.HTTP =>
               if http_index > 0 then
                  http_index := http_index + 1;
                  if http_index > Repo.SSH.total_http_mirrors (my_repo) then
                     http_index := 1;
                  end if;
               end if;
            when Repo.SRV =>
               if srv_index > 0 then
                  srv_index := srv_index + 1;
                  if srv_index > Repo.SSH.total_srv_records (my_repo) then
                     srv_index := 1;
                  end if;
               end if;
            when Repo.NOMIRROR =>
               delay (1.0);
            end case;
         end if;

      end loop;


      if not use_ssh then
         if timestamp /= null then
            declare
               use type Unix.T_epochtime;
               mtime : Unix.T_epochtime := Libfetch.get_file_modification_time (url_components);
            begin
               if mtime > 0 then
                  if mtime <= timestamp.all then
                     Repo.repo_environment (my_repo).Iterate (restore_env'Access);
                     Libfetch.free_url (url_components);
                     Libfetch.fx_close (remote);
                     return RESULT_UPTODATE;
                  else
                     timestamp.all := mtime;
                  end if;
               end if;
            end;
         end if;
         size := int64 (Libfetch.get_fetched_file_size (url_components));
      end if;


      if size <= 0 and then filesize > 0 then
         size := int64 (filesize);
      end if;

      Event.emit_fetch_begin (file_url);
      Event.emit_progress_start ("");
      declare
         one_char   : constant Positive := 1;
         requested  : constant Positive := 8192;
         chars_read : Natural;
         done : int64 := int64 (offset);
         left : int64 := int64 (requested);
      begin
         if size > 0 then
            left := size - done;
         end if;
         loop
            declare
               line : String := Libfetch.fx_fread (remote, one_char, requested, chars_read);
            begin
               if chars_read > 0 then
                  if not Unix.write_to_file_descriptor (dest_fd, line) then
                     Event.emit_errno ("fetch_file_to_fd", "write_chunk", Unix.errno);
                     Repo.repo_environment (my_repo).Iterate (restore_env'Access);
                     Libfetch.free_url (url_components);
                     if not use_ssh then
                        Libfetch.fx_close (remote);
                     end if;
                     return RESULT_FATAL;
                  end if;
                  done := done + int64 (chars_read);
                  if size > 0 then
                     left := left - int64 (chars_read);
                     Event.emit_debug  (4, "Read status:" & done'Img & " over" & size'Img);
                     Event.emit_progress_tick (done, size);
                  else
                     Event.emit_debug (4, "Read status: " & done'Img);
                  end if;
               end if;
            end;
            exit when chars_read = 0;
         end loop;
         if done > 0 then
            Event.emit_progress_tick (done, done);
         else
            if size > 0 then
               Event.emit_progress_tick (size, size);
            else
               Event.emit_progress_tick (1, 1);
            end if;
         end if;
      end;
      Event.emit_fetch_finished (file_url);

      Repo.repo_environment (my_repo).Iterate (restore_env'Access);
      Libfetch.free_url (url_components);

      if not use_ssh then
         if Libfetch.fx_error (remote) then
            Event.emit_error (file_url & ": " & Libfetch.get_last_fetch_error);
         end if;
         Libfetch.fx_close (remote);
         return RESULT_FATAL;
      end if;

      Unix.set_file_times (dest_fd, timestamp.all, timestamp.all);
      return RESULT_OK;

   end fetch_file_to_fd;




end Core.Fetching;
