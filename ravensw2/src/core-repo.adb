--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;

package body Core.Repo is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  repo_url
   --------------------------------------------------------------------
   function repo_url (repo : A_repo) return String is
   begin
      return USS (repo.url);
   end repo_url;


   --------------------------------------------------------------------
   --  repo_name
   --------------------------------------------------------------------
   function repo_name (repo : A_repo) return String is
   begin
      return USS (repo.name);
   end repo_name;


   --------------------------------------------------------------------
   --  repo_pubkey
   --------------------------------------------------------------------
   function repo_pubkey (repo : A_repo) return String is
   begin
      return USS (repo.pubkey);
   end repo_pubkey;


   --------------------------------------------------------------------
   --  repo_fingerprints
   --------------------------------------------------------------------
   function repo_fingerprints (repo : A_repo) return String is
   begin
      return USS (repo.fingerprints);
   end repo_fingerprints;


   --------------------------------------------------------------------
   --  repo_enabled #1
   --------------------------------------------------------------------
   function repo_enabled (repo : A_repo) return String is
   begin
      case repo.enable is
         when False => return "no";
         when True  => return "yes";
      end case;
   end repo_enabled;


   --------------------------------------------------------------------
   --  repo_enabled #2
   --------------------------------------------------------------------
   function repo_enabled (repo : A_repo) return Boolean is
   begin
      return repo.enable;
   end repo_enabled;


   --------------------------------------------------------------------
   --  repo_mirror_type #1
   --------------------------------------------------------------------
   function repo_mirror_type (repo : A_repo) return String is
   begin
      case repo.mirror_type is
         when SRV      => return "SRV";
         when HTTP     => return "HTTP";
         when NOMIRROR => return "NONE";
      end case;
   end repo_mirror_type;


   --------------------------------------------------------------------
   --  repo_mirror_type #2
   --------------------------------------------------------------------
   function repo_mirror_type (repo : A_repo) return A_mirror is
   begin
      return repo.mirror_type;
   end repo_mirror_type;


   --------------------------------------------------------------------
   --  repo_signature_type #1
   --------------------------------------------------------------------
   function repo_signature_type (repo : A_repo) return String is
   begin
      case repo.signature_type is
         when SIG_PUBKEY      => return "PUBKEY";
         when SIG_FINGERPRINT => return "FINGERPRINTS";
         when SIG_NONE        => return "NONE";
      end case;
   end repo_signature_type;


   --------------------------------------------------------------------
   --  repo_signature_type #2
   --------------------------------------------------------------------
   function repo_signature_type (repo : A_repo) return A_signature is
   begin
      return repo.signature_type;
   end repo_signature_type;


   --------------------------------------------------------------------
   --  repo_priority #1
   --------------------------------------------------------------------
   function repo_priority (repo : A_repo) return String is
   begin
      return int2str (Integer (repo.priority));
   end repo_priority;


   --------------------------------------------------------------------
   --  repo_priority #2
   --------------------------------------------------------------------
   function repo_priority (repo : A_repo) return A_priority is
   begin
      return repo.priority;
   end repo_priority;


   --------------------------------------------------------------------
   --  repo_ipv_type #1
   --------------------------------------------------------------------
   function repo_ipv_type (repo : A_repo) return String is
   begin
      case repo.flags is
         when REPO_FLAGS_LIMIT_IPV4 => return "4";
         when REPO_FLAGS_LIMIT_IPV6 => return "6";
         when REPO_FLAGS_DEFAULT    => return "0";
      end case;
   end repo_ipv_type;


   --------------------------------------------------------------------
   --  repo_ipv_type #2
   --------------------------------------------------------------------
   function repo_ipv_type (repo : A_repo) return A_repo_flag is
   begin
      return repo.flags;
   end repo_ipv_type;


   --------------------------------------------------------------------
   --  repo_priority_less_than
   --------------------------------------------------------------------
   function repo_priority_less_than (A, B : Priority_Identity) return Boolean
   is
      --  Display 100 before 90, so it's reverse order (use greater than for "<")
   begin
      if A.priority = B.priority then
         return SU.">" (A.reponame, B.reponame);
      else
         return A.priority > B.priority;
      end if;
   end repo_priority_less_than;


   --------------------------------------------------------------------
   --  count_of_active_repositories
   --------------------------------------------------------------------
   function count_of_active_repositories return Natural
   is
      procedure scan (position : Repository_Crate.Cursor);

      total_active : Natural := 0;

      procedure scan (position : Repository_Crate.Cursor)
      is
         item : A_repo renames Repository_Crate.Element (position);
      begin
         if item.enable then
            total_active := total_active + 1;
         end if;
      end scan;
   begin
      repositories.Iterate (scan'Access);
      return total_active;
   end count_of_active_repositories;


   --------------------------------------------------------------------
   --  total_repositories
   --------------------------------------------------------------------
   function total_repositories return Natural is
   begin
      return Natural (repositories.Length);
   end total_repositories;


   --------------------------------------------------------------------
   --  repository_is_active
   --------------------------------------------------------------------
   function repository_is_active (reponame : String) return Boolean
   is
      reponame_txt : Text := SUS (reponame);
   begin
      if repositories.Contains (reponame_txt) then
         return repositories.Element (reponame_txt).enable;
      else
         return False;
      end if;
   end repository_is_active;


   --------------------------------------------------------------------
   --  first_active_repository
   --------------------------------------------------------------------
   function first_active_repository return String
   is
      procedure list (position : Repos_Priority_Crate.Cursor);

      key   : Text;
      found : Boolean := False;

      procedure list (position : Repos_Priority_Crate.Cursor)
      is
         use type Repository_Crate.Cursor;

         next_cursor : Repository_Crate.Cursor;
      begin
         if not found then
            key := Repos_Priority_Crate.Element (position).reponame;
            --  It's possible repositories are present in priority listing, but not
            --  the repositories crate.  This happens upon loading failure.
            next_cursor := repositories.Find (key);
            if next_cursor /= Repository_Crate.No_Element then
               if Repository_Crate.Element (next_cursor).enable then
                  found := True;
               end if;
            end if;
         end if;
      end list;

   begin
      repositories_order.Iterate (list'Access);
      if found then
         return USS (key);
      else
         return "";
      end if;
   end first_active_repository;


   --------------------------------------------------------------------
   --  get_repository
   --------------------------------------------------------------------
   function get_repository (reponame : String) return A_repo
   is
      reponame_txt : Text := SUS (reponame);
   begin
      if repositories.Contains (reponame_txt) then
         return repositories.Element (reponame_txt);
      else
         raise invalid_repo_name;
      end if;
   end get_repository;


   --------------------------------------------------------------------
   --  joined_priority_order
   --------------------------------------------------------------------
   function joined_priority_order return String
   is
      procedure list (position : Repos_Priority_Crate.Cursor);

      result : Text;

      procedure list (position : Repos_Priority_Crate.Cursor)
      is
         key : Text := Repos_Priority_Crate.Element (position).reponame;
      begin
         if not IsBlank (result) then
            SU.Append (result, LAT.LF);
         end if;
         SU.Append (result, key);
      end list;
   begin
      repositories_order.Iterate (list'Access);
      return USS (result);
   end joined_priority_order;


   --------------------------------------------------------------------
   --  meta_filename
   --------------------------------------------------------------------
   function meta_filename (reponame : String) return String is
   begin
      return reponame & ".meta";
   end meta_filename;


   --------------------------------------------------------------------
   --  count_of_trusted_fingerprints
   --------------------------------------------------------------------
   function count_of_trusted_fingerprints (repo : A_repo) return Natural is
   begin
      return Natural (repo.trusted_fprint.Length);
   end count_of_trusted_fingerprints;



end Core.Repo;
