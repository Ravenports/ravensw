--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core;         use Core;
with Core.Strings; use Core.Strings;
with Core.Pkg;     use Core.Pkg;
with Core.Unix;
with libucl;
with Ucl;

package Core.Config is

   Unsupported_Type : exception;

   repositories       : pkg_repos_crate.Map;
   repositories_order : pkg_repos_priority_crate.Vector;

   type Config_Entry_Type is
     (
      pkg_string,
      pkg_bool,
      pkg_array,
      pkg_int,
      pkg_object
     );

   type Pkg_init_flags is
     (init_none,
      init_use_ipv4,
      init_use_ipv6);

   --  Return True if pkg_ini has already been executed
   function pkg_initialized return Boolean;

   --  Initialize configuration
   function pkg_ini
     (path     : String;
      reposdir : String;
      flags    : Pkg_init_flags;
      dlevel   : ST_Debug_Level;
      options  : String) return Pkg_Error_Type;

   --  Initialize configuration (not restricted to IPv4 or IPv6)
   function pkg_init
     (path     : String;
      reposdir : String;
      dlevel   : ST_Debug_Level;
      options  : String) return Pkg_Error_Type;

   --  Retrieve configuration object given its key
   function pkg_config_get (key : String) return access constant libucl.ucl_object_t;

   --  Retrieve string converted from ucl object given its key`
   function pkg_config_get_string (key : String) return String;

   --  Retrieve boolean converted from ucl object given its key`
   function pkg_config_get_boolean (key : String) return Boolean;

   --  Retrieve signed 64-bit integer converted from ucl object given its key`
   function pkg_config_get_int64 (key : String) return Ucl.int64;

   --  Expand config_object into human-readable text, configuration format
   function pkg_config_dump return String;

   --  Return repo's url
   function pkg_repo_url (repo : T_pkg_repo) return String;

   --  Return repo's name
   function pkg_repo_name (repo : T_pkg_repo) return String;

   --  Return repo's public key
   function pkg_repo_pubkey (repo : T_pkg_repo) return String;

   --  Return repo's fingerprints
   function pkg_repo_fingerprints (repo : T_pkg_repo) return String;

   --  Return repo's enabled as string (yes/no)
   function pkg_repo_enabled (repo : T_pkg_repo) return String;

   --  Return repo's enabled state
   function pkg_repo_enabled (repo : T_pkg_repo) return Boolean;

   --  Return repo's mirror type as string
   function pkg_repo_mirror_type (repo : T_pkg_repo) return String;

   --  Return repo's mirror type natively
   function pkg_repo_mirror_type (repo : T_pkg_repo) return T_mirror_type;

   --  Return repo's signature type as string
   function pkg_repo_signature_type (repo : T_pkg_repo) return String;

   --  Return repo's signature type natively
   function pkg_repo_signature_type (repo : T_pkg_repo) return T_signature;

   --  Return repo's priority type is string
   function pkg_repo_priority_type (repo : T_pkg_repo) return String;

   --  Return repo's priority type natively
   function pkg_repo_priority_type (repo : T_pkg_repo) return T_priority;

   --  Return repo's IP protocol type is string
   function pkg_repo_ipv_type (repo : T_pkg_repo) return String;

   --  Return repo's IP protocol type natively
   function pkg_repo_ipv_type (repo : T_pkg_repo) return T_pkg_repo_flags;

   conf_dbdir          : constant String := "RAVENSW_DBDIR";
   conf_cachedir       : constant String := "RAVENSW_CACHEDIR";
   conf_ravenports     : constant String := "RAVENPORTS";
   conf_rc_scripts     : constant String := "HANDLE_RC_SCRIPTS";
   conf_always_yes     : constant String := "DEFAULT_ALWAYS_YES";
   conf_assume_yes     : constant String := "ASSUME_ALWAYS_YES";
   conf_repos_dir      : constant String := "REPOS_DIR";
   conf_keywords_dir   : constant String := "PLIST_KEYWORDS_DIR";
   conf_syslog         : constant String := "SYSLOG";
   conf_abi            : constant String := "ABI";
   conf_dev_mode       : constant String := "DEVELOPER_MODE";
   conf_fetch_retry    : constant String := "FETCH_RETRY";
   conf_fetch_timeout  : constant String := "FETCH_TIMEOUT";
   conf_deug_scripts   : constant String := "DEBUG_SCRIPTS";
   conf_permissive     : constant String := "PERMISSIVE";
   conf_autoupdate     : constant String := "REPO_AUTOUPDATE";
   conf_nameserver     : constant String := "NAMESERVER";
   conf_user_agent     : constant String := "HTTP_USER_AGENT";
   conf_event_pipe     : constant String := "EVENT_PIPE";
   conf_no_timestamp   : constant String := "UNSET_TIMESTAMP";
   conf_restrict_dir   : constant String := "SSH_RESTRICT_DIR";
   conf_ssh_args       : constant String := "RAVENSW_SSH_ARGS";
   conf_env            : constant String := "RAVENSW_ENV";
   conf_debug_level    : constant String := "DEBUG_LEVEL";
   conf_alias          : constant String := "ALIAS";
   conf_cudf_solver    : constant String := "CUDF_SOLVER";
   conf_sat_solver     : constant String := "SAT_SOLVER";
   conf_run_scripts    : constant String := "RUN_SCRIPTS";
   conf_cs_match       : constant String := "CASE_SENSITIVE_MATCH";
   conf_lock_wait      : constant String := "LOCK_WAIT";
   conf_lock_retries   : constant String := "LOCK_RETRIES";
   conf_sqlite_profile : constant String := "SQLITE_PROFILE";
   conf_workers_count  : constant String := "WORKERS_COUNT";
   conf_read_lock      : constant String := "READ_LOCK";
   conf_ip_version     : constant String := "IP_VERSION";
   conf_automerge      : constant String := "AUTOMERGE";
   conf_version_source : constant String := "VERSION_SOURCE";
   conf_conservative   : constant String := "CONSERVATIVE_UPGRADE";
   conf_create_verbose : constant String := "RAVENSW_CREATE_VERBOSE";
   conf_autoclean      : constant String := "AUTOCLEAN";
   conf_dot_file       : constant String := "DOT_FILE";
   conf_valid_scheme   : constant String := "VALID_URL_SCHEME";
   conf_base_shlibs    : constant String := "ALLOW_BASE_SHLIBS";
   conf_size_limit     : constant String := "WARN_SIZE_LIMIT";
   conf_metalog        : constant String := "METALOG";

private

   parsed        : Boolean := False;
   config_object : access libucl.ucl_object_t;

   type Config_Entry is
      record
         config_type : Config_Entry_Type;
         key         : Text;
         default     : Text;
         description : Text;
      end record;

   type Config_Entry_Array is array (Natural range <>) of Config_Entry;

   config_entries : Config_Entry_Array :=
     (
      (pkg_string,
       SUS (conf_dbdir),
       SUS ("/var/db/ravensw"),
       SUS ("Where the package databases are stored")),

      (pkg_string,
       SUS (conf_cachedir),
       SUS ("/var/cache/ravensw"),
       SUS ("Directory containing cache of downloaded packages")),

      (pkg_string,
       SUS (conf_ravenports),
       SUS ("/var/ravenports/conspiracy"),
       SUS ("Ravenports conspiracy directory (buildsheets)")),

      (pkg_bool,
       SUS (conf_rc_scripts),
       SUS ("NO"),
       SUS ("Automatically handle restarting services")),

      (pkg_bool,
       SUS (conf_always_yes),
       SUS ("NO"),
       SUS ("Default to 'yes' for all ravensw(8) questions")),

      (pkg_bool,
       SUS (conf_assume_yes),
       SUS ("NO"),
       SUS ("Answer 'yes' to all ravensw(8) questions")),

      (pkg_array,
       SUS (conf_repos_dir),
       SUS ("/etc/ravensw/," & install_prefix & "/etc/ravensw/repos/"),
       SUS ("Location of the repository configuration files")),

      (pkg_string,
       SUS (conf_keywords_dir),
       blank,
       SUS ("Directory containing definitions of plist keywords")),

      (pkg_bool,
       SUS (conf_syslog),
       SUS ("YES"),
       SUS ("Log ravensw(8) operations via syslog(3)")),

      (pkg_string,
       SUS (conf_abi),
       SUS ("overwritten"),
       SUS ("Override the automatically detected ABI")),

      (pkg_bool,
       SUS (conf_dev_mode),
       SUS ("NO"),
       SUS ("Add extra strict, pedantic warnings as an aid to package maintainers")),

      (pkg_int,
       SUS (conf_fetch_retry),
       SUS ("3"),
       SUS ("Number of times to retry fetching files")),

      (pkg_int,
       SUS (conf_fetch_timeout),
       SUS ("30"),
       SUS ("Number of seconds before fetch attempt times out")),

      (pkg_bool,
       SUS (conf_deug_scripts),
       SUS ("NO"),
       SUS ("Run shell scripts in verbose mode to facilitate debugging")),

      (pkg_bool,
       SUS (conf_permissive),
       SUS ("NO"),
       SUS ("Permit package installation despite presence of conflicting packages")),

      (pkg_bool,
       SUS (conf_autoupdate),
       SUS ("YES"),
       SUS ("Automatically update repository catalogs prior to package updates")),

      (pkg_string,
       SUS (conf_nameserver),
       blank,
       SUS ("Use this nameserver when looking up addresses")),

      (pkg_string,
       SUS (conf_user_agent),
       SUS ("ravensw/" & progversion),
       SUS ("HTTP User-Agent")),

      (pkg_string,
       SUS (conf_event_pipe),
       blank,
       SUS ("Send all events to the specified fifo or Unix socket")),

      (pkg_bool,
       SUS (conf_no_timestamp),
       SUS ("NO"),
       SUS ("Do not include timestamps in the package")),

      (pkg_string,
       SUS (conf_restrict_dir),
       SUS (""),
       SUS ("Directory in which to restrict the ssh subsystem")),

      (pkg_string,
       SUS (conf_ssh_args),
       blank,
       SUS ("Extras arguments to pass to ssh(1)")),

      (pkg_object,
       SUS (conf_env),
       blank,
       SUS ("Environment variables ravensw(8) will use")),

      (pkg_int,
       SUS (conf_debug_level),
       SUS ("0"),
       SUS ("Level for debug messages")),

      (pkg_object,
       SUS (conf_alias),
       blank,
       SUS ("Command aliases")),

      (pkg_string,
       SUS (conf_cudf_solver),
       blank,
       SUS ("Experimental: tells pkg to use an external CUDF solver")),

      (pkg_string,
       SUS (conf_sat_solver),
       blank,
       SUS ("Experimental: tells pkg to use an external SAT solver")),

      (pkg_bool,
       SUS (conf_run_scripts),
       SUS ("YES"),
       SUS ("Run post/pre actions scripts")),

      (pkg_bool,
       SUS (conf_cs_match),
       SUS ("NO"),
       SUS ("Match package names case sensitively")),

      (pkg_int,
       SUS (conf_lock_wait),
       SUS ("1"),
       SUS ("Wait time (secs) to regain a lock if it is not available")),

      (pkg_int,
       SUS (conf_lock_retries),
       SUS ("5"),
       SUS ("Retries performed to obtain a lock")),

      (pkg_bool,
       SUS (conf_sqlite_profile),
       SUS ("NO"),
       SUS ("Profile sqlite queries")),

      (pkg_int,
       SUS (conf_workers_count),
       SUS ("0"),
       SUS ("How many workers are used for ravensw-repo (hw.ncpu if 0)")),

      (pkg_bool,
       SUS (conf_read_lock),
       SUS ("NO"),
       SUS ("Use read locking for query database")),

      (pkg_int,
       SUS (conf_ip_version),
       SUS ("0"),
       SUS ("Restrict network access to IPv4 or IPv6 only")),

      (pkg_bool,
       SUS (conf_automerge),
       SUS ("YES"),
       SUS ("Automatically merge configuration files")),

      (pkg_string,
       SUS (conf_version_source),
       blank,
       SUS ("Version source for ravensw-config (I, R), default is auto-detect")),

      (pkg_bool,
       SUS (conf_conservative),
       SUS ("YES"),
       SUS ("Prefer repos with higher priority during upgrade")),

      (pkg_bool,
       SUS (conf_create_verbose),
       SUS ("NO"),
       SUS ("Enable verbose mode for 'ravensw create'")),

      (pkg_bool,
       SUS (conf_autoclean),
       SUS ("NO"),
       SUS ("Always cleanup the cache directory after install/upgrade")),

      (pkg_string,
       SUS (conf_dot_file),
       blank,
       SUS ("Save SAT problem to the specified dot file")),

      (pkg_array,
       SUS (conf_valid_scheme),
       SUS ("rsw+http,rsw+https,https,http,file,ssh,ftp,ftps,rsw+ssh,rsw+ftp,rsw+ftps"),
       SUS ("List of valid URL protocols")),

      (pkg_bool,
       SUS (conf_base_shlibs),
       SUS ("NO"),
       SUS ("Enable base libraries analysis")),

      (pkg_int,
       SUS (conf_size_limit),
       SUS ("1048576"),  -- 1 megabyte
       SUS ("Ask user when performing changes for more than this limit")),

      (pkg_string,
       SUS (conf_metalog),
       blank,
       SUS ("Write out the METALOG to the specified file"))
     );

   --  When provided a FIFO or socket, open file descriptor to it.
   --  Closed by Core.Finalize.cleanup
   procedure connect_evpipe (event_pipe : String);

   function convert (obj : access constant libucl.ucl_object_t) return Config_Entry_Type;
   function convert_string_to_ucl_object (cetype  : Config_Entry_Type;
                                          payload : String) return access libucl.ucl_object_t;

   procedure load_repositories (repodir  : String; flags : Pkg_init_flags);
   procedure load_repo_files   (repodir  : String; flags : Pkg_init_flags);
   procedure load_repo_file    (dfd      : Unix.File_Descriptor;
                                repodir  : String;
                                repofile : String;
                                flags    : Pkg_init_flags);
   procedure walk_repo_obj     (fileobj  : access constant libucl.ucl_object_t;
                                filename : String;
                                flags    : Pkg_init_flags);
   procedure add_repo          (repo_obj : access constant libucl.ucl_object_t;
                                reponame : String;
                                flags    : Pkg_init_flags);

end Core.Config;
