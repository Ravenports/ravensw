--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core;         use Core;
with Core.Strings; use Core.Strings;
with Core.Pkg;     use Core.Pkg;
with Core.Ucl;
with libucl;

package Core.Config is

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
      flags    : Pkg_init_flags) return Pkg_Error_Type;

   --  Initialize configuration (not restricted to IPv4 or IPv6)
   function pkg_init
     (path     : String;
      reposdir : String) return Pkg_Error_Type;

   --  Retrieve configure value given it's keep
   function pkg_config_get (key : String) return access constant libucl.ucl_object_t;

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
       SUS ("RAVENSW_DBDIR"),
       SUS ("/var/db/ravensw"),
       SUS ("Where the package databases are stored")),

      (pkg_string,
       SUS ("RAVENSW_CACHEDIR"),
       SUS ("/var/cache/ravensw"),
       SUS ("Directory containing cache of downloaded packages")),

      (pkg_string,
       SUS ("RAVENPORTS"),
       SUS ("/var/ravenports/conspiracy"),
       SUS ("Ravenports conspiracy directory (buildsheets)")),

      (pkg_bool,
       SUS ("HANDLE_RC_SCRIPTS"),
       SUS ("NO"),
       SUS ("Automatically handle restarting services")),

      (pkg_bool,
       SUS ("DEFAULT_ALWAYS_YES"),
       SUS ("NO"),
       SUS ("Default to 'yes' for all ravensw(8) questions")),

      (pkg_bool,
       SUS ("ASSUME_ALWAYS_YES"),
       SUS ("NO"),
       SUS ("Answer 'yes' to all ravensw(8) questions")),

      (pkg_array,
       SUS ("REPOS_DIR"),
       SUS ("/etc/ravensw/," & install_prefix & "/etc/ravensw/repos/"),
       SUS ("Location of the repository configuration files")),

      (pkg_string,
       SUS ("PLIST_KEYWORDS_DIR"),
       blank,
       SUS ("Directory containing definitions of plist keywords")),

      (pkg_bool,
       SUS ("SYSLOG"),
       SUS ("YES"),
       SUS ("Log ravensw(8) operations via syslog(3)")),

      (pkg_string,
       SUS ("ABI"),
       SUS ("TODO: myabi"),
       SUS ("Override the automatically detected ABI")),

      (pkg_bool,
       SUS ("DEVELOPER_MODE"),
       SUS ("NO"),
       SUS ("Add extra strict, pedantic warnings as an aid to package maintainers")),

      (pkg_int,
       SUS ("FETCH_RETRY"),
       SUS ("3"),
       SUS ("Number of times to retry fetching files")),

      (pkg_int,
       SUS ("FETCH_TIMEOUT"),
       SUS ("30"),
       SUS ("Number of seconds before fetch attempt times out")),

      (pkg_bool,
       SUS ("DEBUG_SCRIPTS"),
       SUS ("NO"),
       SUS ("Run shell scripts in verbose mode to facilitate debugging")),

      (pkg_bool,
       SUS ("PERMISSIVE"),
       SUS ("NO"),
       SUS ("Permit package installation despite presence of conflicting packages")),

      (pkg_bool,
       SUS ("REPO_AUTOUPDATE"),
       SUS ("YES"),
       SUS ("Automatically update repository catalogs prior to package updates")),

      (pkg_string,
       SUS ("NAMESERVER"),
       blank,
       SUS ("Use this nameserver when looking up addresses")),

      (pkg_string,
       SUS ("HTTP_USER_AGENT"),
       SUS ("ravensw/" & progversion),
       SUS ("HTTP User-Agent")),

      (pkg_string,
       SUS ("EVENT_PIPE"),
       blank,
       SUS ("Send all events to the specified fifo or Unix socket")),

      (pkg_bool,
       SUS ("UNSET_TIMESTAMP"),
       SUS ("NO"),
       SUS ("Do not include timestamps in the package")),

      (pkg_string,
       SUS ("SSH_RESTRICT_DIR"),
       SUS (""),
       SUS ("Directory in which to restrict the ssh subsystem")),

      (pkg_string,
       SUS ("RAVENSW_SSH_ARGS"),
       blank,
       SUS ("Extras arguments to pass to ssh(1)")),

      (pkg_object,
       SUS ("RAVENSW_ENV"),
       blank,
       SUS ("Environment variables ravensw(8) will use")),

      (pkg_int,
       SUS ("DEBUG_LEVEL"),
       SUS ("0"),
       SUS ("Level for debug messages")),

      (pkg_object,
       SUS ("ALIAS"),
       blank,
       SUS ("Command aliases")),

      (pkg_string,
       SUS ("CUDF_SOLVER"),
       blank,
       SUS ("Experimental: tells pkg to use an external CUDF solver")),

      (pkg_string,
       SUS ("SAT_SOLVER"),
       blank,
       SUS ("Experimental: tells pkg to use an external SAT solver")),

      (pkg_bool,
       SUS ("RUN_SCRIPTS"),
       SUS ("YES"),
       SUS ("Run post/pre actions scripts")),

      (pkg_bool,
       SUS ("CASE_SENSITIVE_MATCH"),
       SUS ("NO"),
       SUS ("Match package names case sensitively")),

      (pkg_int,
       SUS ("LOCK_WAIT"),
       SUS ("1"),
       SUS ("Wait time (secs) to regain a lock if it is not available")),

      (pkg_int,
       SUS ("LOCK_RETRIES"),
       SUS ("5"),
       SUS ("Retries performed to obtain a lock")),

      (pkg_bool,
       SUS ("SQLITE_PROFILE"),
       SUS ("NO"),
       SUS ("Profile sqlite queries")),

      (pkg_int,
       SUS ("WORKERS_COUNT"),
       SUS ("0"),
       SUS ("How many workers are used for ravensw-repo (hw.ncpu if 0)")),

      (pkg_bool,
       SUS ("READ_LOCK"),
       SUS ("NO"),
       SUS ("Use read locking for query database")),

      (pkg_int,
       SUS ("IP_VERSION"),
       SUS ("0"),
       SUS ("Restrict network access to IPv4 or IPv6 only")),

      (pkg_bool,
       SUS ("AUTOMERGE"),
       SUS ("YES"),
       SUS ("Automatically merge configuration files")),

      (pkg_string,
       SUS ("VERSION_SOURCE"),
       blank,
       SUS ("Version source for ravensw-config (I, R), default is auto-detect")),

      (pkg_bool,
       SUS ("CONSERVATIVE_UPGRADE"),
       SUS ("YES"),
       SUS ("Prefer repos with higher priority during upgrade")),

      (pkg_bool,
       SUS ("RAVENSW_CREATE_VERBOSE"),
       SUS ("NO"),
       SUS ("Enable verbose mode for 'ravensw create'")),

      (pkg_bool,
       SUS ("AUTOCLEAN"),
       SUS ("NO"),
       SUS ("Always cleanup the cache directory after install/upgrade")),

      (pkg_string,
       SUS ("DOT_FILE"),
       blank,
       SUS ("Save SAT problem to the specified dot file")),

      (pkg_object,
       SUS ("REPOSITORIES"),
       blank,
       SUS ("Repository config in ravensw.conf")),

      (pkg_array,
       SUS ("VALID_URL_SCHEME"),
       SUS ("rsw+http,rsw+https,https,http,file,ssh,ftp,ftps,rsw+ssh,rsw+ftp,rsw+ftps"),
       SUS ("List of valid URL protocols")),

      (pkg_bool,
       SUS ("ALLOW_BASE_SHLIBS"),
       SUS ("NO"),
       SUS ("Enable base libraries analysis")),

      (pkg_int,
       SUS ("WARN_SIZE_LIMIT"),
       SUS ("1048576"),  -- 1 megabyte
       SUS ("Ask user when performing changes for more than this limit")),

      (pkg_string,
       SUS ("METALOG"),
       blank,
       SUS ("Write out the METALOG to the specified file"))
     );

   --  When provided a FIFO or socket, open file descriptor to it.
   procedure connect_evpipe (event_pipe : String);

   --  close_evpipe (?)

end Core.Config;
