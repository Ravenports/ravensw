--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Core.Config;
with Core.Strings;  use Core.Strings;
with Core.Repo.Common;

package body Core.Repository is

   --------------------------------------------------------------------
   --  pkg_repo_fetch_meta
   --------------------------------------------------------------------
   function pkg_repo_fetch_meta (repo : in out T_pkg_repo; timestamp : T_pkg_timestamp)
                                 return Pkg_Error_Type
   is
      procedure load_meta;
      procedure cleanup;

      dbdirfd : Unix.File_Descriptor;
      fd      : Unix.File_Descriptor;
      metafd  : Unix.File_Descriptor;
      rc      : Pkg_Error_Type;
   begin
      dbdirfd := Config.pkg_get_dbdirfd;
      fd := pkg_repo_fetch_remote_tmp (repo, "meta", timestamp, rc);
      if not Unix.file_connected (fd) then
         return rc;
      end if;
      declare
         filepath : String := Repo.Common.pkg_repo_binary_get_meta (USS (repo.name));
         flags    : T_Open_Flags := (RDONLY => True,
                                     WRONLY => True,
                                     CREAT  => True,
                                     TRUNC  => True,
                                     others => False);
      begin
         metafd := Unix.open_file (dbdirfd, filepath, flags);
         if not Unix.file_connected (metafd) then
            Unix.close_file (fd);
            return rc;
         end if;
      end;

      if repo.signature_type = SIG_PUBKEY then
         --  if ((rc = pkg_repo_archive_extract_check_archive(fd, "meta", repo, metafd)) != EPKG_OK) {
         --  TODO:
         if False then
            close (metafd);
            close (fd);
            return rc;
         end if;
         load_meta;
         cleanup;
         return rc;
      end if;

   end pkg_repo_fetch_meta;

end Core.Repository;
