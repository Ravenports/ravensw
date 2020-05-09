--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Core.Unix;
private with libucl;

package Core.Repo.Read is

   --  Load one or more directories (Unix line-feed delimited "repodirs")
   procedure load_repositories (repodirs : String;
                                flags    : Init_protocol);

private

   procedure walk_repo_obj     (fileobj  : access constant libucl.ucl_object_t;
                                filename : String;
                                flags    : Init_protocol);
   procedure load_repo_file    (dfd      : Unix.File_Descriptor;
                                repodir  : String;
                                repofile : String;
                                flags    : Init_protocol);
   procedure load_repo_files   (repodir  : String; flags : Init_protocol);

   procedure add_repo          (repo_obj : access constant libucl.ucl_object_t;
                                reponame : String;
                                flags    : Init_protocol);

end Core.Repo.Read;
