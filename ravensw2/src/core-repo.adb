--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;

package body Core.Repo is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  repo_url
   --------------------------------------------------------------------
   function repo_url (repo : Repo_Cursor) return String is
   begin
      return USS (Repository_Crate.Element (repo.position).url);
   end repo_url;


   --------------------------------------------------------------------
   --  repo_name
   --------------------------------------------------------------------
   function repo_name (repo : Repo_Cursor) return String is
   begin
      return USS (Repository_Crate.Element (repo.position).name);
   end repo_name;


   --------------------------------------------------------------------
   --  repo_pubkey
   --------------------------------------------------------------------
   function repo_pubkey (repo : Repo_Cursor) return String is
   begin
      return USS (Repository_Crate.Element (repo.position).pubkey);
   end repo_pubkey;


   --------------------------------------------------------------------
   --  repo_fingerprints
   --------------------------------------------------------------------
   function repo_fingerprints (repo : Repo_Cursor) return String is
   begin
      return USS (Repository_Crate.Element (repo.position).fingerprints);
   end repo_fingerprints;


   --------------------------------------------------------------------
   --  repo_enabled #1
   --------------------------------------------------------------------
   function repo_enabled (repo : Repo_Cursor) return String is
   begin
      case Repository_Crate.Element (repo.position).enable is
         when False => return "no";
         when True  => return "yes";
      end case;
   end repo_enabled;


   --------------------------------------------------------------------
   --  repo_enabled #2
   --------------------------------------------------------------------
   function repo_enabled (repo : Repo_Cursor) return Boolean is
   begin
      return Repository_Crate.Element (repo.position).enable;
   end repo_enabled;


   --------------------------------------------------------------------
   --  repo_mirror_type #1
   --------------------------------------------------------------------
   function repo_mirror_type (repo : Repo_Cursor) return String is
   begin
      case Repository_Crate.Element (repo.position).mirror_type is
         when SRV      => return "SRV";
         when HTTP     => return "HTTP";
         when NOMIRROR => return "NONE";
      end case;
   end repo_mirror_type;


   --------------------------------------------------------------------
   --  repo_mirror_type #2
   --------------------------------------------------------------------
   function repo_mirror_type (repo : Repo_Cursor) return A_mirror is
   begin
      return Repository_Crate.Element (repo.position).mirror_type;
   end repo_mirror_type;


   --------------------------------------------------------------------
   --  repo_signature_type #1
   --------------------------------------------------------------------
   function repo_signature_type (repo : Repo_Cursor) return String is
   begin
      case Repository_Crate.Element (repo.position).signature_type is
         when SIG_PUBKEY      => return "PUBKEY";
         when SIG_FINGERPRINT => return "FINGERPRINTS";
         when SIG_NONE        => return "NONE";
      end case;
   end repo_signature_type;


   --------------------------------------------------------------------
   --  repo_signature_type #2
   --------------------------------------------------------------------
   function repo_signature_type (repo : Repo_Cursor) return A_signature is
   begin
      return Repository_Crate.Element (repo.position).signature_type;
   end repo_signature_type;


   --------------------------------------------------------------------
   --  repo_priority #1
   --------------------------------------------------------------------
   function repo_priority (repo : Repo_Cursor) return String is
   begin
      return int2str (Integer (Repository_Crate.Element (repo.position).priority));
   end repo_priority;


   --------------------------------------------------------------------
   --  repo_priority #2
   --------------------------------------------------------------------
   function repo_priority (repo : Repo_Cursor) return A_priority is
   begin
      return Repository_Crate.Element (repo.position).priority;
   end repo_priority;


   --------------------------------------------------------------------
   --  repo_ipv_type #1
   --------------------------------------------------------------------
   function repo_ipv_type (repo : Repo_Cursor) return String is
   begin
      case Repository_Crate.Element (repo.position).flags is
         when REPO_FLAGS_LIMIT_IPV4 => return "4";
         when REPO_FLAGS_LIMIT_IPV6 => return "6";
         when REPO_FLAGS_DEFAULT    => return "0";
      end case;
   end repo_ipv_type;


   --------------------------------------------------------------------
   --  repo_ipv_type #2
   --------------------------------------------------------------------
   function repo_ipv_type (repo : Repo_Cursor) return A_repo_flag is
   begin
      return Repository_Crate.Element (repo.position).flags;
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

      procedure list (position : Repos_Priority_Crate.Cursor) is
      begin
         if not found then
            key := Repos_Priority_Crate.Element (position).reponame;
            --  It's possible repositories are present in priority listing, but not
            --  the repositories crate.  This happens upon loading failure.
            if repositories.Contains (key) then
               if repositories.Element (key).enable then
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
   function get_repository (reponame : String) return Repo_Cursor
   is
      reponame_txt : Text := SUS (reponame);
   begin
      if repositories.Contains (reponame_txt) then
         return (position => repositories.Find (reponame_txt));
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
         key : Text renames Repos_Priority_Crate.Element (position).reponame;
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
   --  ordered_active_repositories
   --------------------------------------------------------------------
   function ordered_active_repositories return Active_Repository_Name_Set.Vector
   is
      procedure list (position : Repos_Priority_Crate.Cursor);

      result : Active_Repository_Name_Set.Vector;

      procedure list (position : Repos_Priority_Crate.Cursor)
      is
         key : Text renames Repos_Priority_Crate.Element (position).reponame;
      begin
         if repositories.Contains (key) then
            if repositories.Element (key).enable then
               result.Append (key);
            end if;
         end if;
      end list;
   begin
      repositories_order.Iterate (list'Access);
      return result;
   end ordered_active_repositories;


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
   function count_of_trusted_fingerprints (repo : Repo_Cursor) return Natural is
   begin
      return Natural (Repository_Crate.Element (repo.position).trusted_fprint.Length);
   end count_of_trusted_fingerprints;


   --------------------------------------------------------------------
   --  repository_exists
   --------------------------------------------------------------------
   function repository_exists (reponame : String) return Boolean is
   begin
      return repositories.Contains (SUS (reponame));
   end repository_exists;


   --------------------------------------------------------------------
   --  repo_meta_digest_format
   --------------------------------------------------------------------
   function repo_meta_digest_format (repo : Repo_Cursor) return Checksum.A_Checksum_Type is
   begin
      return Repository_Crate.Element (repo.position).meta.digest_format;
   end repo_meta_digest_format;


   --------------------------------------------------------------------
   --  repo_environment
   --------------------------------------------------------------------
   function repo_environment (repo : Repo_Cursor) return Pkgtypes.Package_NVPairs.Map is
   begin
      return Repository_Crate.Element (repo.position).env;
   end repo_environment;


   --------------------------------------------------------------------
   --  repo_ssh
   --------------------------------------------------------------------
   function repo_ssh (repo : Repo_Cursor) return Libfetch.Fetch_Stream is
   begin
      return Repository_Crate.Element (repo.position).ssh;
   end repo_ssh;

end Core.Repo;
