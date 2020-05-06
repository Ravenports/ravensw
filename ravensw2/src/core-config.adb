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
            return config_get_boolean (get_ci_key (ci));
         when others =>
            raise config_type_mismatch;
      end case;
   end configuration_value;


   --------------------------------------------------------------------
   --  configuration_value #2 (int64 result)
   --------------------------------------------------------------------
   function configuration_value (ci : Configuration_Item) return int64 is
   begin
      case ci is
         when fetch_retry
            | fetch_timeout
            | debug_level
            | lock_wait
            | lock_retries
            | workers_count
            | ip_version
            | size_limit
            =>
            return config_get_int64 (get_ci_key (ci));
         when others =>
            raise config_type_mismatch;
      end case;
   end configuration_value;


   --------------------------------------------------------------------
   --  configuration_value #3 (string result)
   --------------------------------------------------------------------
   function configuration_value (ci : Configuration_Item) return String is
   begin
      case ci is
         when dbdir
            | cachedir
            | ravenports
            | keywords_dir
            | abi
            | debug_scripts
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
            return config_get_string (get_ci_key (ci));
         when others =>
            raise config_type_mismatch;
      end case;
   end configuration_value;

end Core.Config;
