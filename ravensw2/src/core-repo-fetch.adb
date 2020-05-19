--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Environment_Variables;
with Ada.Containers.Vectors;
with Ada.Directories;

with Core.Context;
with Core.Event;
with Core.Utilities;
with Core.Fetching;

with libarchive;
with libarchive_h;

package body Core.Repo.Fetch is

   package ENV renames Ada.Environment_Variables;
   package DIR renames Ada.Directories;

   --------------------------------------------------------------------
   --  fetch_remote_tmp
   --------------------------------------------------------------------
   function fetch_remote_tmp
     (my_repo   : A_repo;
      filename  : String;
      timestamp : Unix.T_epochtime;
      retcode   : out Action_Result) return Unix.File_Descriptor
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

      extension : constant String := "tzst";
      tmpdir    : constant String := get_tmpdir;
      fex       : constant String := "/" & filename & "." & extension;
      full_url  : constant String := repo_url (my_repo) & fex;
      fd        : Unix.File_Descriptor;
   begin
      if DIR.Exists (tmpdir) then
         case DIR.Kind (tmpdir) is
            when DIR.Directory => null;
            when others =>
               Event.emit_error (tmpdir & " exists but it's not a directory");
               retcode := RESULT_FATAL;
               return Unix.not_connected;
         end case;
      else
         begin
            DIR.Create_Path (tmpdir);
         exception
            when others =>
               Event.emit_error ("Failed to create temporary directory " & tmpdir);
               retcode := RESULT_FATAL;
               return Unix.not_connected;
         end;
      end if;

      declare
         tmp_file  : constant String := tmpdir & fex & "." & Utilities.random_characters;
         tmp_flags : Unix.T_Open_Flags := (RDONLY => True,
                                           WRONLY => True,
                                           CREAT  => True,
                                           TRUNC  => True,
                                           others => False);
      begin
         fd := Unix.open_file (tmp_file, tmp_flags);
         if not Unix.file_connected (fd) then
            Event.emit_error
              ("Could not create temporary file " & tmp_file & ", aborting update.");
            retcode := RESULT_FATAL;
            return Unix.not_connected;
         end if;
         if not Unix.unlink (tmp_file) then
            Event.emit_notice ("Failed to unlink temporary file: " & tmp_file);
         end if;
      end;

      retcode := Fetching.fetch_file_to_fd (reponame  => repo_name (my_repo),
                                            file_url  => full_url,
                                            dest_fd   => fd,
                                            timestamp => timestamp,
                                            offset    => 0,
                                            filesize  => 0);

      if retcode /= RESULT_OK then
         if not Unix.close_file (fd) then
            null;
         end if;
         fd := Unix.not_connected;
      end if;

      return (fd);
   end fetch_remote_tmp;


   --------------------------------------------------------------------
   --  meta_extract_signature_pubkey
   --------------------------------------------------------------------
   function meta_extract_signature_pubkey
     (arc_fd    : Unix.File_Descriptor;
      temp_fd   : Unix.File_Descriptor;
      filename  : String;
      need_sig  : Boolean;
      retcode   : out Action_Result) return String
   is
      arc        : libarchive_h.archive_Access;
      arcent     : libarchive_h.archive_entry_Access;
      data_final : Boolean;
      data_error : Boolean;
      problem    : Boolean;
      result     : Text;
      block_size : constant := 4096;
      funcname   : constant String := "meta_extract_signature_pubkey";
   begin
      retcode := RESULT_FATAL;

      if not Unix.file_connected (arc_fd) then
         Event.emit_error (funcname & ": archive fd is not open");
         return "";
      end if;

      arcent := null;
      arc    := libarchive_h.archive_read_new;
      libarchive.read_support_filter_all (arc);
      libarchive.read_support_format_tar (arc);

      Event.emit_debug (1, "Repo: extracting signature repo");

      begin
         libarchive.read_open_fd (arc, arc_fd, block_size);
      exception
         when libarchive.archive_error =>
            Event.emit_error (funcname & ": failed to read archive fd");
            libarchive.read_close (arc);
            libarchive.read_free (arc);
            return "";
      end;

      problem := False;
      loop
         exit when not libarchive.read_next_header (arc, arcent, data_final, data_error);

         declare
            fpath : constant String := libarchive.entry_pathname (arcent);
            len   : libarchive.arc64;
         begin
            if need_sig and then fpath = "signature" then
               len := libarchive.entry_size (arcent);
               result := SUS (libarchive.read_data (arc, len));
            elsif fpath = filename then
               if not libarchive.read_data_into_file_descriptor (arc, temp_fd) then
                  Event.emit_error (funcname & ": read into temp fd");
                  problem := True;
                  exit;
               end if;
            end if;
         exception
            when libarchive.archive_error =>
               Event.emit_error (funcname & ": read_data() " & libarchive.error_string (arc));
               problem := True;
               exit;
         end;
      end loop;
      if data_error then
         Event.emit_error (funcname & ": " & libarchive.error_string (arc));
         problem := True;
      end if;

      if not Unix.close_file (arc_fd) then
         Event.emit_error (funcname & ": failed to close archive fd");
      end if;
      libarchive.read_close (arc);
      libarchive.read_free (arc);

      if not problem then
         retcode := RESULT_OK;
      end if;
      return USS (result);
   end meta_extract_signature_pubkey;


   --------------------------------------------------------------------
   --  archive_extract_archive
   --------------------------------------------------------------------
--     function archive_extract_archive
--       (my_repo   : A_repo;
--        fd        : Unix.File_Descriptor;
--        filename  : String;
--        dest_fd   : Unix.File_Descriptor;
--        signature : out Signature_Certificate) return Action_Result
--     is
--     begin
--        Event.emit_debug
--          (1, "Repo: extracting " & filename & " of " & Repo.repo_name (my_repo) & " repository");
--
--        case Repo.repo_signature_type (my_repo) is
--           when SIG_PUBKEY => null;
--           when SIG_FINGERPRINT => null;
--           when SIG_NONE =>
--        end case;
--     end archive_extract_archive;
--
--
--     --------------------------------------------------------------------
--     --  archive_extract_check_archive
--     --------------------------------------------------------------------
--     function archive_extract_check_archive
--       (my_repo   : A_repo;
--        fd        : Unix.File_Descriptor;
--        filename  : String;
--        dest_fd   : Unix.File_Descriptor) return Action_Result
--     is
--        rc  : Action_Result := RESULT_OK;
--        ret : Action_Result := RESULT_OK;
--     begin
--
--
--        	struct sig_cert *sc = NULL, *s, *stmp;
--  	int ret, rc;
--
--  	ret = rc = EPKG_OK;
--
--  	if (pkg_repo_archive_extract_archive(fd, file, repo, dest_fd, &sc)
--  			!= EPKG_OK)
--  		return (EPKG_FATAL);
--
--  	if (pkg_repo_signature_type(repo) == SIG_PUBKEY) {
--  		if (pkg_repo_key(repo) == NULL) {
--  			pkg_emit_error("No PUBKEY defined. Removing "
--  			    "repository.");
--  			rc = EPKG_FATAL;
--  			goto out;
--  		}
--  		if (sc == NULL) {
--  			pkg_emit_error("No signature found in the repository.  "
--  					"Can not validate against %s key.", pkg_repo_key(repo));
--  			rc = EPKG_FATAL;
--  			goto out;
--  		}
--  		/*
--  		 * Here are dragons:
--  		 * 1) rsa_verify is NOT rsa_verify_cert
--  		 * 2) siglen must be reduced by one to support this legacy method
--  		 *
--  		 * by @bdrewery
--  		 */
--  		ret = rsa_verify(pkg_repo_key(repo), sc->sig, sc->siglen - 1,
--  		    dest_fd);
--  		if (ret != EPKG_OK) {
--  			pkg_emit_error("Invalid signature, "
--  					"removing repository.");
--  			rc = EPKG_FATAL;
--  			goto out;
--  		}
--  	}
--  	else if (pkg_repo_signature_type(repo) == SIG_FINGERPRINT) {
--  		HASH_ITER(hh, sc, s, stmp) {
--  			ret = rsa_verify_cert(s->cert, s->certlen, s->sig, s->siglen,
--  				dest_fd);
--  			if (ret == EPKG_OK && s->trusted) {
--  				break;
--  			}
--  			ret = EPKG_FATAL;
--  		}
--  		if (ret != EPKG_OK) {
--  			pkg_emit_error("No trusted certificate has been used "
--  			    "to sign the repository");
--  			rc = EPKG_FATAL;
--  			goto out;
--  		}
--  	}
--
--  out:
--  	return rc;
--     end archive_extract_check_archive;
--
--
--     --------------------------------------------------------------------
--     --  fetch_meta
--     --------------------------------------------------------------------
--     function fetch_meta
--       (my_repo   : A_repo;
--        timestamp : Unix.T_epochtime) return Action_Result
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
--        fd := fetch_remote_tmp (my_repo, "meta", timestamp, rc);
--        if not Unix.file_connected (fd) then
--           return rc;
--        end if;
--
--        declare
--           filepath : String := Repo.meta_filename (repo_name (my_repo));
--           flags    : Unix.T_Open_Flags := (RDONLY => True,
--                                            WRONLY => True,
--                                            CREAT  => True,
--                                            TRUNC  => True,
--                                            others => False);
--        begin
--           metafd := Unix.open_file (dbdirfd, filepath, flags);
--           if not Unix.file_connected (metafd) then
--              if Unix.close_file (fd) then
--                 null;
--              end if;
--              return rc;
--           end if;
--        end;
--
--        if Repo.repo_signature_type (my_repo) = SIG_PUBKEY then
--
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
--     end fetch_meta;

end Core.Repo.Fetch;
