--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core;         use Core;
with Core.Strings; use Core.Strings;

package Core.Config is

   config_type_mismatch : exception;

   type Configuration_Item is
     (dbdir, cachedir, ravenports, rc_scripts, always_yes, assume_yes, repos_dir,
      keywords_dir, syslog, abi, dev_mode, fetch_retry, fetch_timeout, debug_scripts,
      permissive, autoupdate, nameserver, user_agent, event_pipe, no_timestamp,
      restrict_dir, ssh_args, environ, debug_level, alias, cudf_solver, sat_solver,
      run_scripts, cs_match, lock_wait, lock_retries, sqlite_profile, workers_count,
      read_lock, ip_version, automerge, version_source, conservative, create_verbose,
      autoclean, dot_file, valid_scheme, base_shlibs, size_limit, metalog_file
     );

   --  Overloaded functions that return value of configuration given enum.
   --  If the type is a mismatch then config_type_mismatch exception is thrown.
   function configuration_value (ci : Configuration_Item) return String;
   function configuration_value (ci : Configuration_Item) return Boolean;
   function configuration_value (ci : Configuration_Item) return int64;

   --  Retrieve configuration item key given enum
   function get_ci_key (ci : Configuration_Item) return String;

private

   --  Current values of configuration items
   configuration_entry : array (Configuration_Item'Range) of Text;

   type Config_Entry_Type is
     (pkg_string,
      pkg_bool,
      pkg_array,
      pkg_int,
      pkg_object);

   function config_get_type (ci : Configuration_Item) return Config_Entry_Type;

   function config_get_description (ci : Configuration_Item) return String;

   function config_get_default_value (ci : Configuration_Item) return String;

end Core.Config;
