--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core;         use Core;
with Core.Strings; use Core.Strings;

private with libucl;

package Core.Config is

   config_type_mismatch : exception;

   type Init_protocol is (init_none, init_use_ipv4, init_use_ipv6);
   type Configuration_Item is
     (dbdir, cachedir, ravenports, rc_scripts, always_yes, assume_yes, repos_dir,
      keywords_dir, syslog, abi, dev_mode, fetch_retry, fetch_timeout, debug_scripts,
      permissive, autoupdate, nameserver, user_agent, event_pipe, no_timestamp,
      restrict_dir, ssh_args, env, debug_level, alias, cudf_solver, sat_solver,
      run_scripts, cs_match, lock_wait, lock_retries, sqlite_profile, workers_count,
      read_lock, ip_version, automerge, version_source, conservative, create_verbose,
      autoclean, dot_file, valid_scheme, base_shlibs, size_limit, metalog
     );

   --  Overloaded functions that return value of configuration given enum.
   --  If the type is a mismatch then config_type_mismatch exception is thrown.
   function configuration_value (ci : Configuration_Item) return String;
   function configuration_value (ci : Configuration_Item) return Boolean;
   function configuration_value (ci : Configuration_Item) return int64;

   --  Expand config_object into human-readable text, configuration format
   function config_dump return String;

private

   config_object : access libucl.ucl_object_t;

   --  Retrieve configuration item key given enum
   function get_ci_key (ci : Configuration_Item) return String;

   --  Retrieve configuration object given its key
   function config_get (key : String) return access constant libucl.ucl_object_t;

   --  Retrieve string converted from ucl object given its key`
   function config_get_string (key : String) return String;

   --  Retrieve boolean converted from ucl object given its key`
   function config_get_boolean (key : String) return Boolean;

   --  Retrieve signed 64-bit integer converted from ucl object given its key`
   function config_get_int64 (key : String) return int64;

end Core.Config;
