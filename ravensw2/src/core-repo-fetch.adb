--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Environment_Variables;
with Ada.Containers.Vectors;
with Ada.Directories;

with Core.Context;
with Core.Event;
with Core.Utilities;
with Core.Fetching;
with Core.Repo.Keys;
with Core.Repo.Meta;
with Core.Checksum;
with Core.RSA;

with libarchive;
with libarchive_h;

package body Core.Repo.Fetch is

   package ENV renames Ada.Environment_Variables;
   package DIR renames Ada.Directories;


   --------------------------------------------------------------------
   --  fetch_remote_tmp
   --------------------------------------------------------------------
   function fetch_remote_tmp
     (my_repo   : in out A_repo;
      filename  : String;
      timestamp : access Unix.T_epochtime;
      retcode   : out Action_Result) return Unix.File_Descriptor
   is
      extension : constant String := "tzst";
      fex       : constant String := "/" & filename & "." & extension;
      tmp_file  : constant String := temporary_file_name (fex);
      full_url  : constant String := repo_url (my_repo) & fex;
      fd        : Unix.File_Descriptor;
   begin
      if not temporary_directory_available then
         retcode := RESULT_FATAL;
         return Unix.not_connected;
      end if;

      fd := open_temporary_file (tmp_file);
      if not Unix.file_connected (fd) then
         Event.emit_error ("Could not create temporary file " & tmp_file & ", aborting update.");
         retcode := RESULT_FATAL;
         return Unix.not_connected;
      end if;
      if not Unix.unlink (tmp_file) then
         Event.emit_notice ("Failed to unlink temporary file: " & tmp_file);
      end if;

      retcode := Fetching.fetch_file_to_fd (my_repo   => my_repo,
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
   --  meta_extract_to_file_descriptor
   --------------------------------------------------------------------
   function meta_extract_to_file_descriptor
     (arc_fd    : Unix.File_Descriptor;
      target_fd : Unix.File_Descriptor;
      filename  : String) return Action_Result
   is
      arc        : libarchive_h.archive_Access;
      arcent     : aliased libarchive_h.archive_entry_Access;
      data_final : Boolean;
      data_error : Boolean;
      acquired   : Boolean := False;
      block_size : constant := 4096;
      funcname   : constant String := "meta_extract_to_file_descriptor";
   begin
      if not Unix.file_connected (arc_fd) then
         Event.emit_error (funcname & ": archive fd is not open");
         return RESULT_FATAL;
      end if;

      arcent := null;
      arc    := libarchive_h.archive_read_new;
      libarchive.read_support_filter_all (arc);
      libarchive.read_support_format_tar (arc);

      Event.emit_debug (1, "Repo: extracting " & filename & " from archive");

      begin
         libarchive.read_open_fd (arc, arc_fd, block_size);
      exception
         when libarchive.archive_error =>
            Event.emit_error (funcname & ": failed to read archive fd");
            libarchive.read_close (arc);
            libarchive.read_free (arc);
            return RESULT_FATAL;
      end;

      loop
         exit when not libarchive.read_next_header (arc    => arc,
                                                    arcent => arcent'Unchecked_Access,
                                                    error  => data_error,
                                                    final  => data_final);
         begin
            if libarchive.entry_pathname (arcent) = filename then
               if libarchive.read_data_into_file_descriptor (arc, target_fd) then
                  acquired := True;
               else
                  Event.emit_error (funcname & ": read into target fd");
               end if;
               exit;
            end if;
         exception
            when libarchive.archive_error =>
               Event.emit_error (funcname & ": read_data() " & libarchive.error_string (arc));
               exit;
         end;
      end loop;

      if data_error then
         Event.emit_error (funcname & ": " & libarchive.error_string (arc));
      end if;

      if not Unix.close_file (target_fd) then
         Event.emit_error (funcname & ": failed to close target fd");
      end if;
      libarchive.read_close (arc);
      libarchive.read_free (arc);

      if acquired then
         return RESULT_OK;
      else
         return RESULT_FATAL;
      end if;
   end meta_extract_to_file_descriptor;


   --------------------------------------------------------------------
   --  meta_extract_signature_pubkey
   --------------------------------------------------------------------
   function meta_extract_signature_pubkey
     (arc_fd    : Unix.File_Descriptor;
      target_fd : Unix.File_Descriptor;
      target    : String;
      retcode   : out Action_Result) return String
   is
      arc        : libarchive_h.archive_Access;
      arcent     : aliased libarchive_h.archive_entry_Access;
      data_final : Boolean;
      data_error : Boolean;
      failure    : Boolean := False;
      tgt_found  : Boolean := False;
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

      Event.emit_debug (1, "Repo: extracting signature of repository");

      begin
         libarchive.read_open_fd (arc, arc_fd, block_size);
      exception
         when libarchive.archive_error =>
            Event.emit_error (funcname & ": failed to read archive fd");
            libarchive.read_close (arc);
            libarchive.read_free (arc);
            return "";
      end;

      loop
         exit when failure;
         exit when not libarchive.read_next_header (arc    => arc,
                                                    arcent => arcent'Unchecked_Access,
                                                    error  => data_error,
                                                    final  => data_final);
         declare
            len : libarchive.arc64;
            entry_file : constant String := libarchive.entry_pathname (arcent);
         begin
            if entry_file = "signature" then
               len := libarchive.entry_size (arcent);
               result := SUS (libarchive.read_data (arc, len));
               retcode := RESULT_OK;
            elsif entry_file = target then
               if libarchive.read_data_into_file_descriptor (arc, target_fd) then
                  tgt_found := True;
               else
                  Event.emit_error (funcname & ": read into target fd");
                  failure := True;
               end if;
            end if;
         exception
            when libarchive.archive_error =>
               Event.emit_error (funcname & ": read_data() " & libarchive.error_string (arc));
               failure := True;
         end;
      end loop;
      if data_error then
         Event.emit_error (funcname & ": " & libarchive.error_string (arc));
      end if;
      libarchive.read_close (arc);
      libarchive.read_free (arc);

      if not tgt_found then
         Event.emit_debug (1, funcname & ": target file " & SQ (target) & " not found");
         retcode := RESULT_FATAL;
      end if;

      return USS (result);
   end meta_extract_signature_pubkey;


   --------------------------------------------------------------------
   --  meta_extract_signature_fingerprints
   --------------------------------------------------------------------
   function meta_extract_signature_fingerprints
     (arc_fd    : Unix.File_Descriptor;
      target_fd : Unix.File_Descriptor;
      target    : String;
      fingerprints : out Set_File_Contents.Vector) return Action_Result
   is
      arc        : libarchive_h.archive_Access;
      arcent     : aliased libarchive_h.archive_entry_Access;
      data_final : Boolean;
      data_error : Boolean;
      problem    : Boolean := False;
      tgt_found  : Boolean := False;
      block_size : constant := 4096;
      funcname   : constant String := "meta_extract_signature_fingerprints";
   begin
      fingerprints.Clear;

      if not Unix.file_connected (arc_fd) then
         Event.emit_error (funcname & ": archive fd is not open");
         return RESULT_FATAL;
      end if;

      arcent := null;
      arc    := libarchive_h.archive_read_new;
      libarchive.read_support_filter_all (arc);
      libarchive.read_support_format_tar (arc);

      Event.emit_debug (1, "Repo: extracting fingerprints of repository");

      begin
         libarchive.read_open_fd (arc, arc_fd, block_size);
      exception
         when libarchive.archive_error =>
            Event.emit_error (funcname & ": failed to read archive fd");
            libarchive.read_close (arc);
            libarchive.read_free (arc);
            return RESULT_FATAL;
      end;

      loop
         exit when problem;
         exit when not libarchive.read_next_header (arc    => arc,
                                                    arcent => arcent'Unchecked_Access,
                                                    error  => data_error,
                                                    final  => data_final);
         declare
            fpath : constant String := libarchive.entry_pathname (arcent);
            len   : libarchive.arc64;
         begin
            if file_extension_matches (fpath, ".sig") or else
              file_extension_matches (fpath, ".pub")
            then
               len := libarchive.entry_size (arcent);
               fingerprints.Append (SUS (libarchive.read_data (arc, len)));
            elsif fpath = target then
               if libarchive.read_data_into_file_descriptor (arc, target_fd) then
                  tgt_found := True;
               else
                  Event.emit_error (funcname & ": read into target fd");
                  problem := True;
               end if;
            end if;
         exception
            when libarchive.archive_error =>
               Event.emit_error (funcname & ": read_data() " & libarchive.error_string (arc));
               problem := True;
         end;
      end loop;
      if data_error then
         Event.emit_error (funcname & ": " & libarchive.error_string (arc));
      end if;
      libarchive.read_close (arc);
      libarchive.read_free (arc);

      if not tgt_found then
         Event.emit_debug (1, funcname & ": target file " & SQ (target) & " not found");
         return RESULT_FATAL;
      end if;

      if problem then
         return RESULT_FATAL;
      else
         return RESULT_OK;
      end if;
   end meta_extract_signature_fingerprints;


   --------------------------------------------------------------------
   --  file_extension_matches
   --------------------------------------------------------------------
   function file_extension_matches (filename, extension : String) return Boolean is
   begin
      return trails (filename, extension);
   end file_extension_matches;


   --------------------------------------------------------------------
   --  archive_extract_archive
   --------------------------------------------------------------------
   function archive_extract_archive
     (my_repo  : in out A_repo;
      fd       : Unix.File_Descriptor;
      filename : String;
      dest_fd  : Unix.File_Descriptor;
      cert_set : out Set_Signature_Certificates.Vector) return Action_Result
   is
      procedure insert_print (position : Set_File_Contents.Cursor);

      seek_err  : constant String := "archive_extract_archive: failed to reset fd for reading.";
      insertion : Action_Result := RESULT_OK;

      procedure insert_print (position : Set_File_Contents.Cursor)
      is
         binary_print : Text renames Set_File_Contents.Element (position);
         certificate  : Signature_Certificate;
      begin
         if parse_sigkey (USS (binary_print), certificate) = RESULT_OK then
            cert_set.Append (certificate);
         else
            insertion := RESULT_FATAL;
         end if;
      end insert_print;
   begin
      Event.emit_debug
        (1, "Repo: extracting " & filename & " of " & Repo.repo_name (my_repo) & " repository");

      if not Unix.reset_file_for_reading (fd) then
         Event.emit_error (seek_err);
         return RESULT_FATAL;
      end if;

      cert_set.Clear;
      case Repo.repo_signature_type (my_repo) is
         when SIG_PUBKEY =>
            declare
               rc     : Action_Result;
               pubkey : String := meta_extract_signature_pubkey (fd, dest_fd, filename, rc);
               cert   : Signature_Certificate;
            begin
               if rc = RESULT_OK then
                  cert.name    := SUS ("signature");
                  cert.sc_sign := SUS (pubkey);
                  cert_set.Append (cert);
               end if;
            end;
         when SIG_NONE =>
            declare
               rc : Action_Result;
            begin
               rc := meta_extract_to_file_descriptor (fd, dest_fd, filename);
               if rc /= RESULT_OK then
                  Event.emit_error ("Repo SIG_NONE extract failed: " & filename);
                  return RESULT_FATAL;
               end if;
            end;
         when SIG_FINGERPRINT =>
            declare
               rc : Action_Result;
               fingerprints : Set_File_Contents.Vector;
            begin
               rc := meta_extract_signature_fingerprints (fd, dest_fd, filename, fingerprints);
               if rc /= RESULT_OK then
                  Event.emit_error ("Repo SIG_FINGERPRINT extract failed");
                  return RESULT_FATAL;
               end if;
               fingerprints.Iterate (insert_print'Access);
               if insertion /= RESULT_OK then
                  Event.emit_error ("Repo SIG_FINGERPRINT insertion failed");
                  return RESULT_FATAL;
               end if;
               if not check_fingerprints (my_repo, cert_set) then
                  return RESULT_FATAL;
               end if;
            end;
      end case;

      if not Unix.reset_file_for_reading (fd) then
         Event.emit_error (seek_err);
         return RESULT_FATAL;
      end if;
      if not Unix.file_connected (dest_fd) then
         if not Unix.reset_file_for_reading (dest_fd) then
            Event.emit_error (seek_err);
            return RESULT_FATAL;
         end if;
      end if;

      return RESULT_OK;
   end archive_extract_archive;


   --------------------------------------------------------------------
   --  parse_sigkey
   --------------------------------------------------------------------
   function parse_sigkey  (encoded_sigkey : String;
                           signature      : out Signature_Certificate) return Action_Result
   is
      --  Format: [A][B][C][D][E]
      --  [A] is one byte, contains 0 or 1, and represents signature type
      --  [B] is 4 bytes, represents length of following filename
      --  [C] is eval(B) bytes, contains filename
      --  [D] is 4 bytes, represents length of following signature
      --  [E] is eval(D) bytes, contains signature

      too_short : constant String := "parse_sigkey(): key is too short ("
                  & int2str (encoded_sigkey'Length) & " bytes)";
      type Key_Format is range 0 .. 1;
      format    : Key_Format;
      file_size : Utilities.uint32;
      sig_size  : Utilities.uint32;
      ndx       : Integer;
      minimum   : Integer;
   begin
      if encoded_sigkey'Length < 26 then
         --  Less than 1 byte filename and 16 byte key.  This is way too short
         Event.emit_error (too_short);
         return RESULT_FATAL;
      end if;

      declare
         B : Integer;
      begin
         B := Character'Pos (encoded_sigkey (encoded_sigkey'First));
         if B < 0 or else B > 1 then
            Event.emit_error (int2str (B) &  "is not a valid type for fingerprint format");
            return RESULT_FATAL;
         end if;
         format := Key_Format (B);
      end;

      ndx := encoded_sigkey'First + 1;
      file_size := Utilities.conv2int (encoded_sigkey (ndx .. ndx + 3));

      minimum := Integer (file_size) + 9;
      if encoded_sigkey'Length < minimum then
         Event.emit_error (too_short & " (needs at least " & int2str (minimum) & ")");
         return RESULT_FATAL;
      end if;
      signature.name := SUS (encoded_sigkey (ndx + 4 .. ndx + 4 + Integer (file_size)));

      ndx := encoded_sigkey'First + Integer (file_size) + 5;
      sig_size := Utilities.conv2int (encoded_sigkey (ndx .. ndx + 3));

      minimum := Integer (file_size) + Integer (sig_size) + 9;
      if encoded_sigkey'Length < minimum then
         Event.emit_error (too_short & " (requires " & int2str (minimum) & ")");
         return RESULT_FATAL;
      end if;

      case format is
         when 0 => signature.sc_sign := SUS (encoded_sigkey (ndx + 4 .. minimum));
         when 1 => signature.sc_cert := SUS (encoded_sigkey (ndx + 4 .. minimum));
      end case;
      return RESULT_OK;
   end parse_sigkey;


   --------------------------------------------------------------------
   --  check_fingerprints
   --------------------------------------------------------------------
   function check_fingerprints
     (my_repo  : in out A_repo;
      cert_set : in out Set_Signature_Certificates.Vector) return Boolean
   is
      procedure scan_certificate (position : Set_Signature_Certificates.Cursor);
      procedure update_trust (Element : in out Signature_Certificate);

      nbgood  : Natural := 0;
      aborted : Boolean := False;

      procedure update_trust (Element : in out Signature_Certificate) is
      begin
         Element.trusted := True;
         nbgood := nbgood + 1;
      end update_trust;

      procedure scan_certificate (position : Set_Signature_Certificates.Cursor)
      is
         procedure scan_meta_key (mk_position : A_cert_crate.Cursor);
         procedure update_cert (Element : in out Signature_Certificate);

         cert : Signature_Certificate renames Set_Signature_Certificates.Element (position);
         mk_found : Boolean := False;
         cert_data : Text;

         procedure scan_meta_key (mk_position : A_cert_crate.Cursor)
         is
            meta_key : Meta_Certificate renames A_cert_crate.Element (mk_position);
         begin
            if not mk_found and then equivalent (meta_key.name, cert.name) then
               cert_data := meta_key.pubkey;
               mk_found := True;
            end if;
         end scan_meta_key;

         procedure update_cert (Element : in out Signature_Certificate) is
         begin
           Element.sc_cert := cert_data;
         end update_cert;
      begin
         if not aborted then
            if IsBlank (cert.sc_cert) then
               if IsBlank (cert.sc_sign) then
                  Event.emit_error ("check_fingerprints(): undefined signature method");
                  aborted := True;
               else
                  --  Check meta keys
                  my_repo.meta.cert_set.Iterate (scan_meta_key'Access);
                  if mk_found then
                     cert_set.Update_Element (position, update_cert'Access);
                  else
                     Event.emit_error ("No key with name " & USS (cert.name) & " has been found");
                     aborted := True;
                  end if;
               end if;
            end if;
            if not aborted then
               declare
                  procedure scan_revoked (fp_position : A_Fingerprint_crate.Cursor);
                  procedure scan_trusted (fp_position : A_Fingerprint_crate.Cursor);

                  hash : constant String :=
                    checksum.checksum_data (USS (cert.sc_cert), Checksum.HASH_TYPE_SHA256_HEX);
                  revoked : Boolean := False;

                  procedure scan_revoked (fp_position : A_Fingerprint_crate.Cursor)
                  is
                     cert : A_fingerprint renames A_Fingerprint_crate.Element (fp_position);
                  begin
                     if equivalent (cert.hash, hash) then
                        revoked := True;
                     end if;
                  end scan_revoked;

                  procedure scan_trusted (fp_position : A_Fingerprint_crate.Cursor)
                  is
                     cert : A_fingerprint renames A_Fingerprint_crate.Element (fp_position);
                  begin
                     if equivalent (cert.hash, hash) then
                        cert_set.Update_Element (position, update_trust'Access);
                     end if;
                  end scan_trusted;
               begin
                  my_repo.revoked_fprint.Iterate (scan_revoked'Access);
                  if revoked then
                     Event.emit_error ("At least one of the certificates has been revoked");
                     aborted := True;
                  end if;
                  if not aborted then
                     my_repo.trusted_fprint.Iterate (scan_trusted'Access);
                  end if;
               end;
            end if;
         end if;
      end scan_certificate;
   begin
      if cert_set.Is_Empty then
         Event.emit_error ("No signature found");
      end if;

      --  load fingerprints
      if my_repo.trusted_fprint.Is_Empty then
         if Repo.Keys.load_fingerprints (my_repo) /= RESULT_OK then
            return False;
         end if;
      end if;

      cert_set.Iterate (scan_certificate'Access);
      if nbgood = 0 then
         Event.emit_error ("No trusted public keys found");
         return False;
      end if;
      return True;

   end check_fingerprints;


   --------------------------------------------------------------------
   --  fingerprint_certs_verified
   --------------------------------------------------------------------
   function fingerprint_certs_verified
     (metafd   : Unix.File_Descriptor;
      cert_set : Set_Signature_Certificates.Vector) return Action_Result
   is
      procedure scan (position : Set_Signature_Certificates.Cursor);

      trusted_found : Boolean := False;
      fatal         : Boolean := False;

      procedure scan (position : Set_Signature_Certificates.Cursor)
      is
         x : Signature_Certificate renames Set_Signature_Certificates.Element (position);
      begin
         if not trusted_found and not fatal then
            if Core.RSA.rsa_verify_cert (key       => USS (x.sc_cert),
                                         signature => USS (x.sc_sign),
                                         fd        => metafd) = RESULT_OK
            then
               if x.trusted then
                  trusted_found := True;
               end if;
            else
               fatal := True;
            end if;
         end if;
      end scan;
   begin
      cert_set.Iterate (scan'Access);
      if trusted_found then
         return RESULT_OK;
      else
         Event.emit_error ("No trusted certificate has been used to sign the repository");
         return RESULT_FATAL;
      end if;
   end fingerprint_certs_verified;


   --------------------------------------------------------------------
   --  archive_extract_check_archive
   --------------------------------------------------------------------
   function archive_extract_check_archive
     (my_repo   : in out A_repo;
      fd        : Unix.File_Descriptor;
      filename  : String;
      dest_fd   : Unix.File_Descriptor) return Action_Result
   is
      rc  : Action_Result := RESULT_OK;
      ret : Action_Result := RESULT_OK;
      sc  : Set_Signature_Certificates.Vector;
   begin
      if archive_extract_archive (my_repo  => my_repo,
                                  fd       => fd,
                                  filename => filename,
                                  dest_fd  => dest_fd,
                                  cert_set => sc) /= RESULT_OK
      then
         return RESULT_FATAL;
      end if;

      case repo_signature_type (my_repo) is
         when SIG_PUBKEY =>
            if IsBlank (my_repo.pubkey) then
               Event.emit_error ("No PUBKEY defined. Removing repository.");
               return RESULT_FATAL;
            end if;
            if sc.Is_Empty then
               Event.emit_error
                 ("No signature found in the repository. Can not validate against "
                  & USS (my_repo.pubkey) & " key.");
               return RESULT_FATAL;
            end if;
            --  There has to be only one certificate in the vector;
            if Core.RSA.deprecated_rsa_verify (key_file  => USS (my_repo.pubkey),
                                               signature => USS (sc.First_Element.sc_sign),
                                               fd        => dest_fd) = RESULT_OK
            then
               return RESULT_OK;
            else
               Event.emit_error ("Invalid signature, removing repository.");
               return RESULT_FATAL;
            end if;
         when SIG_FINGERPRINT =>
            return fingerprint_certs_verified (dest_fd, sc);
         when SIG_NONE =>
            return RESULT_OK;
      end case;
   end archive_extract_check_archive;


   --------------------------------------------------------------------
   --  fetch_meta
   --------------------------------------------------------------------
   function fetch_meta
     (my_repo   : in out A_repo;
      timestamp : access Unix.T_epochtime) return Action_Result
   is
      procedure silent_close (this_fd : Unix.File_Descriptor);
      procedure erase_metafile;
      procedure close_sockets;

      dbdirfd : Unix.File_Descriptor;
      fd      : Unix.File_Descriptor;
      metafd  : Unix.File_Descriptor;
      rc      : Action_Result;
      sc      : Set_Signature_Certificates.Vector;

      procedure silent_close (this_fd : Unix.File_Descriptor)
      is
         res : Boolean;
      begin
         res := Unix.close_file (this_fd);
      end silent_close;

      procedure erase_metafile
      is
         rel_filename : String := Repo.meta_filename (repo_name (my_repo));
         res : Boolean;
      begin
         res := Unix.unlink (dbdirfd, rel_filename, False);
      end erase_metafile;

      procedure close_sockets is
      begin
         silent_close (fd);
         silent_close (metafd);
      end close_sockets;

   begin
      dbdirfd := Context.reveal_db_directory_fd;
      fd := fetch_remote_tmp (my_repo, "meta", timestamp, rc);
      if not Unix.file_connected (fd) then
         return rc;
      end if;

      declare
         filepath : String := Repo.meta_filename (repo_name (my_repo));
         flags    : Unix.T_Open_Flags := (RDONLY => True,
                                          WRONLY => True,
                                          CREAT  => True,
                                          TRUNC  => True,
                                          others => False);
      begin
         metafd := Unix.open_file (dbdirfd, filepath, flags);
         if not Unix.file_connected (metafd) then
            silent_close (fd);
            Event.emit_error ("failed to open " & filepath & " from db directory fd");
            return RESULT_FATAL;
         end if;
      end;

      case Repo.repo_signature_type (my_repo) is
         when SIG_PUBKEY =>
            if archive_extract_check_archive (my_repo  => my_repo,
                                              fd       => fd,
                                              filename => "meta",
                                              dest_fd  => metafd) /= RESULT_OK
            then
               rc := RESULT_FATAL;
            end if;

            if rc = RESULT_OK then
               my_repo.meta := Repo.Meta.meta_load (metafd, rc);
               close_sockets;
            else
               close_sockets;
               erase_metafile;
               my_repo.meta := Repo.Meta.meta_set_default;
            end if;
            return rc;

         when SIG_FINGERPRINT =>

            --
            --  For fingerprints we cannot just load pubkeys as they could be in metafile itself
            --  To do it, we parse meta and for each unloaded pubkey we try to return
            --  a corresponding key from meta file.
            --
            if archive_extract_archive (my_repo  => my_repo,
                                        fd       => fd,
                                        filename => "meta",
                                        dest_fd  => metafd,
                                        cert_set => sc) /= RESULT_OK
            then
               erase_metafile;
               close_sockets;
               return RESULT_FATAL;
            end if;

            silent_close (fd);

            --  load fingerprints
            if my_repo.trusted_fprint.Is_Empty then
               if Repo.Keys.load_fingerprints (my_repo) /= RESULT_OK then
                  silent_close (metafd);
                  erase_metafile;
                  return RESULT_FATAL;
               end if;
            end if;

            declare
               procedure update_cert (Element : in out Signature_Certificate);

               new_cert : Text;

               procedure update_cert (Element : in out Signature_Certificate) is
               begin
                  Element.sc_cert := new_cert;
               end update_cert;

               procedure scan (position : Set_Signature_Certificates.Cursor)
               is
                  x : Signature_Certificate renames Set_Signature_Certificates.Element (position);
                  tmp : Text;
                  rc  : Action_Result;
               begin
                  tmp := Repo.Keys.extract_public_key (metafd, USS (x.name), rc);
                  if rc = RESULT_OK then
                     new_cert := tmp;
                     sc.Update_Element (Position, update_cert'Access);
                  end if;
               end scan;
            begin
               sc.Iterate (scan'Access);
            end;

            if not check_fingerprints (my_repo, sc) then
               silent_close (metafd);
               erase_metafile;
               return RESULT_FATAL;
            end if;

            if fingerprint_certs_verified (metafd, sc) = RESULT_OK then
               my_repo.meta := Repo.Meta.meta_load (metafd, rc);
               silent_close (metafd);
               return rc;
            else
               my_repo.meta := Repo.Meta.meta_set_default;
               silent_close (metafd);
               erase_metafile;
               return RESULT_FATAL;
            end if;

         when SIG_NONE =>

            my_repo.meta := Repo.Meta.meta_load (metafd, rc);
            close_sockets;
            return rc;
      end case;

   end fetch_meta;


   --------------------------------------------------------------------
   --  get_tmpdir
   --------------------------------------------------------------------
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


   --------------------------------------------------------------------
   --  temporary_directory_available
   --------------------------------------------------------------------
   function temporary_directory_available return Boolean
   is
      tmpdir : constant String := get_tmpdir;
   begin
      if DIR.Exists (tmpdir) then
         case DIR.Kind (tmpdir) is
            when DIR.Directory => null;
            when others =>
               Event.emit_error (tmpdir & " exists but it's not a directory");
               return False;
         end case;
      else
         begin
            DIR.Create_Path (tmpdir);
         exception
            when others =>
               Event.emit_error ("Failed to create temporary directory " & tmpdir);
               return False;
         end;
      end if;
      return True;
   end temporary_directory_available;


   --------------------------------------------------------------------
   --  temporary_file_name
   --------------------------------------------------------------------
   function temporary_file_name (basename : String) return String
   is
      tmpdir    : constant String := get_tmpdir;
      tmp_file  : constant String := tmpdir & basename & "." & Utilities.random_characters;
   begin
      return tmp_file;
   end temporary_file_name;


   --------------------------------------------------------------------
   --  open_temporary_file
   --------------------------------------------------------------------
   function open_temporary_file (filename : String) return Unix.File_Descriptor
   is
      tmp_flags : Unix.T_Open_Flags := (RDONLY => True,
                                        WRONLY => True,
                                        CREAT  => True,
                                        TRUNC  => True,
                                        others => False);
   begin
      return Unix.open_file (filename, tmp_flags);
   end open_temporary_file;


   --------------------------------------------------------------------
   --  fetch_remote_extract_to_temporary_file
   --------------------------------------------------------------------
   function fetch_remote_extract_to_temporary_file
     (my_repo   : in out A_repo;
      filename  : String;
      timestamp : access Unix.T_epochtime;
      file_size : out int64;
      retcode   : out Action_Result) return String
   is
      procedure silent_close_fd (fd : Unix.File_Descriptor);

      fd       : Unix.File_Descriptor;
      dest_fd  : Unix.File_Descriptor;
      tmp_file : constant String := temporary_file_name (filename);
      NOFILE   : constant String := "";

      procedure silent_close_fd (fd : Unix.File_Descriptor) is
      begin
         if Unix.close_file (fd) then
            null;
         end if;
      end silent_close_fd;
   begin
      file_size := 0;
      fd := fetch_remote_tmp (my_repo, filename, timestamp, retcode);
      if not Unix.file_connected (fd) then
         return NOFILE;
      end if;

      retcode := RESULT_FATAL;

      --  fetch_remote_tmp already established a temporary directory
      dest_fd := open_temporary_file (tmp_file);
      if not Unix.file_connected (fd) then
         Event.emit_error ("Could not create temporary file " & tmp_file & ", aborting update.");
         silent_close_fd (fd);
         return NOFILE;
      end if;

      if archive_extract_check_archive (my_repo  => my_repo,
                                        fd       => fd,
                                        filename => filename,
                                        dest_fd  => dest_fd) /= RESULT_OK
      then
         silent_close_fd (dest_fd);
         silent_close_fd (fd);
         return NOFILE;
      end if;

      --  Thus removing archived file as well
      silent_close_fd (fd);
      declare
         fsize : Unix.T_filesize;
      begin
         fsize := Unix.get_file_size (dest_fd);
         file_size := int64 (fsize);
      exception
         when Unix.bad_stat =>
            silent_close_fd (dest_fd);
            return NOFILE;
      end;
      silent_close_fd (dest_fd);
      retcode := RESULT_OK;
      return tmp_file;
   end fetch_remote_extract_to_temporary_file;

end Core.Repo.Fetch;
