--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Environment_Variables;
with Ada.Directories;

with Core.Context;
with Core.Event;
with Core.Utilities;
with Core.Fetching;

package body Core.Repo.Fetch is

   package ENV renames Ada.Environment_Variables;
   package DIR renames Ada.Directories;

   --------------------------------------------------------------------
   --  fetch_remote_tmp
   --------------------------------------------------------------------
   function fetch_remote_tmp
     (my_repo   : A_repo;
      filename  : String;
      extension : String;
      timestamp : Unix.T_epochtime;
      rc        : out Action_Result) return Unix.File_Descriptor
   is
      function get_tmpdir return String;
      function get_tmpdir return String
      is
         key_TMPDIR : constant String := "TMPDIR";
      begin
         if ENV.Exists (key_TMPDIR) then
            return ENV.Value (key_TMPDIR);
         else
            return "/tmp";
         end if;
      end get_tmpdir;

      tmpdir   : constant String := get_tmpdir;
      fex      : constant String := "/" & filename & "." & extension;
      full_url : constant String := repo_url (my_repo) & fex;
      fd       : Unix.File_Descriptor;
   begin
      if DIR.Exists (tmpdir) then
         case DIR.Kind (tmpdir) is
            when DIR.Directory => null;
            when others =>
               Event.emit_error (tmpdir & " exists but it's not a directory");
               rc := RESULT_FATAL;
               return Unix.not_connected;
         end case;
      else
         begin
            DIR.Create_Path (tmpdir);
         exception
            when others =>
               Event.emit_error ("Failed to create temporary directory " & tmpdir);
               rc := RESULT_FATAL;
               return Unix.not_connected;
         end;
      end if;

      declare
         tmp_file  : constant String := tmpdir & fex & "." & Utilities.random_characters;
         tmp_flags : Unix.T_Open_Flags;
      begin
         tmp_flags.CREAT := True;
         tmp_flags.RDONLY := True;
         tmp_flags.WRONLY := True;
         fd := Unix.open_file (tmp_file, tmp_flags);
         if not Unix.file_connected (fd) then
            Event.emit_error
              ("Could not create temporary file " & tmp_file & ", aborting update.");
            rc := RESULT_FATAL;
            return Unix.not_connected;
         end if;
         if not Unix.unlink (tmp_file) then
            Event.emit_notice ("Failed to unlink temporary file: " & tmp_file);
         end if;
      end;

      rc := Fetching.fetch_file_to_fd (reponame  => repo_name (my_repo),
                                       file_url  => full_url,
                                       dest_fd   => fd,
                                       timestamp => timestamp,
                                       offset    => 0,
                                       filesize  => 0);

      if rc /= RESULT_OK then
         if not Unix.close_file (fd) then
            null;
         end if;
         fd := Unix.not_connected;
      end if;

      return (fd);
   end fetch_remote_tmp;


   --------------------------------------------------------------------
   --  fetch_meta
   --------------------------------------------------------------------
--     function fetch_meta
--       (reponame : String;
--        timestamp : Pkgtypes.Package_Timestamp) return Action_Result
--     is
--        procedure load_meta;
--        procedure cleanup;
--
--        dbdirfd : Unix.File_Descriptor;
--        fd      : Unix.File_Descriptor;
--        metafd  : Unix.File_Descriptor;
--        rc      : Action_Result;
--     begin
--        dbdirfd := Context.reveal_db_directory_fd;
--
--
--        fd := pkg_repo_fetch_remote_tmp (repo, "meta", timestamp, rc);
--        if not Unix.file_connected (fd) then
--           return rc;
--        end if;
--        declare
--           filepath : String := Repo.Common.pkg_repo_binary_get_meta (USS (repo.name));
--           flags    : T_Open_Flags := (RDONLY => True,
--                                       WRONLY => True,
--                                       CREAT  => True,
--                                       TRUNC  => True,
--                                       others => False);
--        begin
--           metafd := Unix.open_file (dbdirfd, filepath, flags);
--           if not Unix.file_connected (metafd) then
--              Unix.close_file (fd);
--              return rc;
--           end if;
--        end;
--
--        if repo.signature_type = SIG_PUBKEY then
--           --  if ((rc = pkg_repo_archive_extract_check_archive(fd, "meta", repo, metafd))
--  != EPKG_OK) {
--           --  TODO:
--           if False then
--              close (metafd);
--              close (fd);
--              return rc;
--           end if;
--           load_meta;
--           cleanup;
--           return rc;
--        end if;
--     end fetch_remote_tmp;

end Core.Repo.Fetch;
