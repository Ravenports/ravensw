--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core;         use Core;
with Core.Strings; use Core.Strings;

private with libucl;

package Core.Config is

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

   --  Retrieve string converted from ucl object given its key`
   function config_get_string (key : String) return String;

   --  Retrieve boolean converted from ucl object given its key`
   function config_get_boolean (key : String) return Boolean;

   --  Retrieve signed 64-bit integer converted from ucl object given its key`
   function config_get_int64 (key : String) return int64;

   --  Expand config_object into human-readable text, configuration format
   function config_dump return String;

private

   config_object : access libucl.ucl_object_t;

   --  Retrieve configuration object given its key
   function config_get (key : String) return access constant libucl.ucl_object_t;



end Core.Config;
