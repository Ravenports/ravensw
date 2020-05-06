--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Object;
with Ucl;


package body Core.Config is

   package COB renames Core.Object;

   --------------------------------------------------------------------
   --  config_get
   --------------------------------------------------------------------
   function config_get (key : String) return access constant libucl.ucl_object_t is
   begin
      return Ucl.ucl_object_find_key (config_object, key);
   end config_get;


   --------------------------------------------------------------------
   --  config_get_string
   --------------------------------------------------------------------
   function config_get_string (key : String) return String is
   begin
      return COB.pkg_object_string (config_get (key));
   end config_get_string;


   --------------------------------------------------------------------
   --  config_get_boolean
   --------------------------------------------------------------------
   function config_get_boolean (key : String) return Boolean is
   begin
      return COB.pkg_object_bool (config_get (key));
   end config_get_boolean;


   --------------------------------------------------------------------
   --  config_get_int64
   --------------------------------------------------------------------
   function config_get_int64 (key : String) return int64 is
   begin
      return COB.pkg_object_int (config_get (key));
   end config_get_int64;


   --------------------------------------------------------------------
   --  config_dump
   --------------------------------------------------------------------
   function config_dump return String is
   begin
      return Object.pkg_object_dump (config_object);
   end config_dump;


   --------------------------------------------------------------------
   --  get_ci_key
   --------------------------------------------------------------------
   function get_ci_key (ci : Configuration_Item) return String is
   begin
      case ci is
         when dbdir          => return "RAVENSW_DBDIR";
         when cachedir       => return "RAVENSW_CACHEDIR";
         when ravenports     => return "RAVENPORTS";
         when rc_scripts     => return "HANDLE_RC_SCRIPTS";
         when always_yes     => return "DEFAULT_ALWAYS_YES";
         when assume_yes     => return "ASSUME_ALWAYS_YES";
         when repos_dir      => return "REPOS_DIR";
         when keywords_dir   => return "PLIST_KEYWORDS_DIR";
         when syslog         => return "SYSLOG";
         when abi            => return "ABI";
         when dev_mode       => return "DEVELOPER_MODE";
         when fetch_retry    => return "FETCH_RETRY";
         when fetch_timeout  => return "FETCH_TIMEOUT";
         when debug_scripts  => return "DEBUG_SCRIPTS";
         when permissive     => return "PERMISSIVE";
         when autoupdate     => return "REPO_AUTOUPDATE";
         when nameserver     => return "NAMESERVER";
         when user_agent     => return "HTTP_USER_AGENT";
         when event_pipe     => return "EVENT_PIPE";
         when no_timestamp   => return "UNSET_TIMESTAMP";
         when restrict_dir   => return "SSH_RESTRICT_DIR";
         when ssh_args       => return "RAVENSW_SSH_ARGS";
         when env            => return "RAVENSW_ENV";
         when debug_level    => return "DEBUG_LEVEL";
         when alias          => return "ALIAS";
         when cudf_solver    => return "CUDF_SOLVER";
         when sat_solver     => return "SAT_SOLVER";
         when run_scripts    => return "RUN_SCRIPTS";
         when cs_match       => return "CASE_SENSITIVE_MATCH";
         when lock_wait      => return "LOCK_WAIT";
         when lock_retries   => return "LOCK_RETRIES";
         when sqlite_profile => return "SQLITE_PROFILE";
         when workers_count  => return "WORKERS_COUNT";
         when read_lock      => return "READ_LOCK";
         when ip_version     => return "IP_VERSION";
         when automerge      => return "AUTOMERGE";
         when version_source => return "VERSION_SOURCE";
         when conservative   => return "CONSERVATIVE_UPGRADE";
         when create_verbose => return "RAVENSW_CREATE_VERBOSE";
         when autoclean      => return "AUTOCLEAN";
         when dot_file       => return "DOT_FILE";
         when valid_scheme   => return "VALID_URL_SCHEME";
         when base_shlibs    => return "ALLOW_BASE_SHLIBS";
         when size_limit     => return "WARN_SIZE_LIMIT";
         when metalog        => return "METALOG";
      end case;
   end get_ci_key;


   --------------------------------------------------------------------
   --  configuration_value #1 (Boolean result)
   --------------------------------------------------------------------
   function configuration_value (ci : Configuration_Item) return Boolean is
   begin
      if config_get_type (ci) = pkg_bool then
         return config_get_boolean (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end configuration_value;


   --------------------------------------------------------------------
   --  configuration_value #2 (int64 result)
   --------------------------------------------------------------------
   function configuration_value (ci : Configuration_Item) return int64 is
   begin
      if config_get_type (ci) = pkg_int then
         return config_get_int64 (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end configuration_value;


   --------------------------------------------------------------------
   --  configuration_value #3 (string result)
   --------------------------------------------------------------------
   function configuration_value (ci : Configuration_Item) return String is
   begin
      if config_get_type (ci) = pkg_string then
         return config_get_string (get_ci_key (ci));
      else
         raise config_type_mismatch;
      end if;
   end configuration_value;


   --------------------------------------------------------------------
   --  config_get_type
   --------------------------------------------------------------------
   function config_get_type (ci : Configuration_Item) return Config_Entry_Type is
   begin
      case ci is
         when rc_scripts
            | always_yes
            | assume_yes
            | syslog
            | dev_mode
            | debug_scripts
            | permissive
            | autoupdate
            | no_timestamp
            | run_scripts
            | cs_match
            | sqlite_profile
            | read_lock
            | automerge
            | conservative
            | create_verbose
            | autoclean
            | base_shlibs
            =>
            return pkg_bool;
         when fetch_retry
            | fetch_timeout
            | debug_level
            | lock_wait
            | lock_retries
            | workers_count
            | ip_version
            | size_limit
            =>
            return pkg_int;
         when dbdir
            | cachedir
            | ravenports
            | keywords_dir
            | abi
            | nameserver
            | user_agent
            | event_pipe
            | restrict_dir
            | ssh_args
            | cudf_solver
            | sat_solver
            | version_source
            | dot_file
            | metalog
            =>
            return pkg_string;
         when env
            | alias
            =>
            return pkg_object;
         when repos_dir
            | valid_scheme
            =>
            return pkg_array;
      end case;
   end config_get_type;


   --------------------------------------------------------------------
   --  config_get_type
   --------------------------------------------------------------------
   function config_get_description (ci : Configuration_Item) return String is
   begin
      case ci is
         when dbdir          => return "Where the package databases are stored";
         when cachedir       => return "Directory containing cache of downloaded packages";
         when ravenports     => return "Ravenports conspiracy directory (buildsheets)";
         when rc_scripts     => return "Automatically handle restarting services";
         when always_yes     => return "Default to 'yes' for all ravensw(8) questions";
         when assume_yes     => return "Answer 'yes' to all ravensw(8) questions";
         when repos_dir      => return "Location of the repository configuration files";
         when keywords_dir   => return "Directory containing definitions of plist keywords";
         when syslog         => return "Log ravensw(8) operations via syslog(3)";
         when abi            => return "Override the automatically detected ABI";
         when dev_mode       => return "Add strict, pedantic warnings as an aid to maintainers";
         when fetch_retry    => return "Number of times to retry fetching files";
         when fetch_timeout  => return "Number of seconds before fetch attempt times out";
         when debug_scripts  => return "Run shell scripts in verbose mode to facilitate debugging";
         when permissive     => return "Permit package installation despite conflicting packages";
         when autoupdate     => return "Update repository catalogs prior to package updates";
         when nameserver     => return "Use this nameserver when looking up addresses";
         when user_agent     => return "HTTP User-Agent";
         when event_pipe     => return "Send all events to the specified fifo or Unix socket";
         when no_timestamp   => return "Do not include timestamps in the package";
         when restrict_dir   => return "Directory in which to restrict the ssh subsystem";
         when ssh_args       => return "Extras arguments to pass to ssh(1)";
         when env            => return "Environment variables ravensw(8) will use";
         when debug_level    => return "Level for debug messages";
         when alias          => return "Command aliases";
         when cudf_solver    => return "Experimental: tells pkg to use an external CUDF solver";
         when sat_solver     => return "Experimental: tells pkg to use an external SAT solver";
         when run_scripts    => return "Run post/pre actions scripts";
         when cs_match       => return "Match package names case sensitively";
         when lock_wait      => return "Wait time (secs) to regain a lock if it is not available";
         when lock_retries   => return "Retries performed to obtain a lock";
         when sqlite_profile => return "Profile sqlite queries";
         when workers_count  => return "How many workers are used for ravensw-repo (hw.ncpu if 0)";
         when read_lock      => return "Use read locking for query database";
         when ip_version     => return "Restrict network access to IPv4 or IPv6 only";
         when automerge      => return "Automatically merge configuration files";
         when version_source => return "Version source for ravensw-config, default is auto-detect";
         when conservative   => return "Prefer repos with higher priority during upgrade";
         when create_verbose => return "Enable verbose mode for 'ravensw create'";
         when autoclean      => return "Always cleanup the cache directory after install/upgrade";
         when dot_file       => return "Save SAT problem to the specified dot file";
         when valid_scheme   => return "List of valid URL protocols";
         when base_shlibs    => return "Enable base libraries analysis";
         when size_limit     => return "Ask user when performing changes for more than this limit";
         when metalog        => return "Write out the METALOG to the specified file";
      end case;
   end config_get_description;


   --------------------------------------------------------------------
   --  config_get_default_value
   --------------------------------------------------------------------
   function config_get_default_value (ci : Configuration_Item) return String is
   begin
      case ci is
         when dbdir          => return "/var/db/ravensw";
         when cachedir       => return "/var/cache/ravensw";
         when ravenports     => return "/var/ravenports/conspiracy";
         when rc_scripts     => return "NO";
         when always_yes     => return "NO";
         when assume_yes     => return "NO";
         when repos_dir      => return "/etc/ravensw/," & install_prefix & "/etc/ravensw/repos/";
         when keywords_dir   => return "";
         when syslog         => return "YES";
         when abi            => return "overwritten";
         when dev_mode       => return "NO";
         when fetch_retry    => return "3";
         when fetch_timeout  => return "30";
         when debug_scripts  => return "NO";
         when permissive     => return "NO";
         when autoupdate     => return "YES";
         when nameserver     => return "";
         when user_agent     => return "ravensw/" & progversion;
         when event_pipe     => return "";
         when no_timestamp   => return "NO";
         when restrict_dir   => return "";
         when ssh_args       => return "";
         when env            => return "";
         when debug_level    => return "0";
         when alias          => return "";
         when cudf_solver    => return "";
         when sat_solver     => return "";
         when run_scripts    => return "YES";
         when cs_match       => return "NO";
         when lock_wait      => return "1";
         when lock_retries   => return "5";
         when sqlite_profile => return "NO";
         when workers_count  => return "0";
         when read_lock      => return "NO";
         when ip_version     => return "0";
         when automerge      => return "YES";
         when version_source => return "";
         when conservative   => return "YES";
         when create_verbose => return "NO";
         when autoclean      => return "NO";
         when dot_file       => return "";
         when valid_scheme   => return "rsw+http,rsw+https,https,http,file,ssh,ftp,ftps," &
                                       "rsw+ssh,rsw+ftp,rsw+ftps";
         when base_shlibs    => return "NO";
         when size_limit     => return "1048576";  -- 1 megabyte
         when metalog        => return "";
      end case;
   end config_get_default_value;

end Core.Config;
