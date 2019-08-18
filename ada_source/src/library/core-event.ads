--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg;  use Core.Pkg;

package Core.Event is

   type Event_Type is
     (
      --  informational
      PKG_EVENT_INSTALL_BEGIN,
      PKG_EVENT_INSTALL_FINISHED,
      PKG_EVENT_DEINSTALL_BEGIN,
      PKG_EVENT_DEINSTALL_FINISHED,
      PKG_EVENT_UPGRADE_BEGIN,
      PKG_EVENT_UPGRADE_FINISHED,
      PKG_EVENT_EXTRACT_BEGIN,
      PKG_EVENT_EXTRACT_FINISHED,
      PKG_EVENT_DELETE_FILES_BEGIN,
      PKG_EVENT_DELETE_FILES_FINISHED,
      PKG_EVENT_ADD_DEPS_BEGIN,
      PKG_EVENT_ADD_DEPS_FINISHED,
      PKG_EVENT_FETCHING,
      PKG_EVENT_FETCH_BEGIN,
      PKG_EVENT_FETCH_FINISHED,
      PKG_EVENT_UPDATE_ADD,
      PKG_EVENT_UPDATE_REMOVE,
      PKG_EVENT_INTEGRITYCHECK_BEGIN,
      PKG_EVENT_INTEGRITYCHECK_FINISHED,
      PKG_EVENT_INTEGRITYCHECK_CONFLICT,
      PKG_EVENT_NEWPKGVERSION,
      PKG_EVENT_NOTICE,
      PKG_EVENT_DEBUG,
      PKG_EVENT_INCREMENTAL_UPDATE,
      PKG_EVENT_QUERY_YESNO,
      PKG_EVENT_QUERY_SELECT,
      --  PKG_EVENT_SANDBOX_CALL,
      --  PKG_EVENT_SANDBOX_GET_STRING,
      PKG_EVENT_PROGRESS_START,
      PKG_EVENT_PROGRESS_TICK,
      PKG_EVENT_BACKUP,
      PKG_EVENT_RESTORE,
      --  errors
      PKG_EVENT_ERROR,
      PKG_EVENT_ERRNO,
      PKG_EVENT_ARCHIVE_COMP_UNSUP,
      PKG_EVENT_ALREADY_INSTALLED,
      PKG_EVENT_FAILED_CKSUM,
      PKG_EVENT_CREATE_DB_ERROR,
      PKG_EVENT_LOCKED,
      PKG_EVENT_REQUIRED,
      PKG_EVENT_MISSING_DEP,
      PKG_EVENT_NOREMOTEDB,
      PKG_EVENT_NOLOCALDB,
      PKG_EVENT_FILE_MISMATCH,
      PKG_EVENT_DEVELOPER_MODE,
      PKG_EVENT_NOT_FOUND,
      PKG_EVENT_NEW_ACTION,
      PKG_EVENT_MESSAGE,
      PKG_EVENT_FILE_MISSING,
      PKG_EVENT_CLEANUP_CALLBACK_REGISTER,
      PKG_EVENT_CLEANUP_CALLBACK_UNREGISTER,
      PKG_EVENT_CONFLICTS
     );

   type Clean_Callback is access function (data : Text) return Boolean;

   type pkg_event (this_event : Event_Type) is
      record
         event : Event_Type := this_event;

         case this_event is
            when PKG_EVENT_ERROR |
                 PKG_EVENT_NOTICE |
                 PKG_EVENT_MESSAGE |
                 PKG_EVENT_DEVELOPER_MODE |
                 PKG_EVENT_PROGRESS_START =>
               message : Text;
            when PKG_EVENT_ERRNO =>
               err_function : Text;
               err_argument : Text;
               err_number   : Integer;
            when PKG_EVENT_UPDATE_ADD |
                 PKG_EVENT_UPDATE_REMOVE =>
               total : Natural;
               done  : Natural;
            when PKG_EVENT_FETCH_BEGIN |
                 PKG_EVENT_FETCH_FINISHED =>
               url : Text;
            when PKG_EVENT_ALREADY_INSTALLED |
                 PKG_EVENT_INSTALL_BEGIN |
                 PKG_EVENT_DEINSTALL_BEGIN |
                 PKG_EVENT_DEINSTALL_FINISHED |
                 PKG_EVENT_EXTRACT_BEGIN |
                 PKG_EVENT_EXTRACT_FINISHED |
                 PKG_EVENT_DELETE_FILES_BEGIN |
                 PKG_EVENT_DELETE_FILES_FINISHED |
                 PKG_EVENT_ADD_DEPS_BEGIN |
                 PKG_EVENT_ADD_DEPS_FINISHED |
                 PKG_EVENT_LOCKED =>
               pkg : T_pkg;
            when PKG_EVENT_INSTALL_FINISHED |
                 PKG_EVENT_UPGRADE_BEGIN |
                 PKG_EVENT_UPGRADE_FINISHED =>
               new_pkg   : T_pkg;
               old_pkg   : T_pkg;
            when PKG_EVENT_MISSING_DEP =>
               dep_pkg   : T_pkg;
               dep_dep   : T_pkg_dep;
            when PKG_EVENT_REQUIRED =>
               req_pkg   : T_pkg;
               force     : Boolean;
            when PKG_EVENT_FILE_MISMATCH =>
               fmm_pkg   : T_pkg;
               fmm_file  : T_pkg_file;
               newsum    : Text;
            when PKG_EVENT_FILE_MISSING =>
               miss_pkg  : T_pkg;
               miss_file : T_pkg_file;
            when PKG_EVENT_NOT_FOUND =>
               pkg_name  : Text;
            when PKG_EVENT_NOREMOTEDB =>
               repo      : Text;
            when PKG_EVENT_INTEGRITYCHECK_CONFLICT =>
               pkg_uid   : Text;
               pkg_path  : Text;
               conflicts : pkg_event_conflict_crate.Vector;
            when PKG_EVENT_INTEGRITYCHECK_FINISHED =>
               conflicting : Natural;
            when PKG_EVENT_INCREMENTAL_UPDATE =>
               reponame  : Text;
               processed : Natural;
            when PKG_EVENT_DEBUG =>
               debug_level : ST_Debug_Level;
               debug_msg   : Text;
            when PKG_EVENT_QUERY_YESNO =>
               qyesno_msg  : Text;
               qyesno_deft : Natural;
            when PKG_EVENT_QUERY_SELECT =>
               query_msg    : Text;
               query_items  : access pkg_query_items_crate.Vector;
               query_ncount : Natural;
               query_deft   : Natural;
            when PKG_EVENT_PROGRESS_TICK =>
               prog_current : T_progress_tick;
               prog_total   : T_progress_tick;
            when PKG_EVENT_CONFLICTS =>
               conflict_pkg1 : T_pkg;
               conflict_pkg2 : T_pkg;
               conflict_path : Text;
            when PKG_EVENT_CLEANUP_CALLBACK_REGISTER |
                 PKG_EVENT_CLEANUP_CALLBACK_UNREGISTER =>
               cleanup_callback      : Clean_Callback;
               cleanup_callback_data : Text;
            when PKG_EVENT_FETCHING |
                 PKG_EVENT_INTEGRITYCHECK_BEGIN |
                 PKG_EVENT_NEWPKGVERSION |
                 PKG_EVENT_BACKUP |
                 PKG_EVENT_RESTORE |
                 PKG_EVENT_ARCHIVE_COMP_UNSUP |
                 PKG_EVENT_FAILED_CKSUM |
                 PKG_EVENT_CREATE_DB_ERROR |
                 PKG_EVENT_NOLOCALDB |
                 PKG_EVENT_NEW_ACTION =>
               null;
         end case;
      end record;

   type Event_Callback is access function (event : pkg_event; data : Text) return Boolean;

   procedure pkg_event_register (callback : Event_Callback; callback_data : Text);

   procedure pkg_emit_message           (message : Text);
   procedure pkg_emit_error             (message : Text);
   procedure pkg_emit_notice            (message : Text);
   procedure pkg_emit_developer_mode    (message : Text);
   procedure pkg_emit_with_strerror     (message : Text);
   procedure pkg_emit_errno             (function_name : Text;
                                         arguments     : Text;
                                         error_number  : Integer);
   procedure pkg_emit_already_installed (pkg : T_pkg);
   procedure pkg_emit_fetch_begin       (url : Text);
   procedure pkg_emit_fetch_finished    (url : Text);
   procedure pkg_emit_update_add        (total, done : Natural);
   procedure pkg_emit_update_remove     (total, done : Natural);
   procedure pkg_emit_install_begin     (pkg : T_pkg);
   procedure pkg_emit_install_finished  (new_pkg : T_pkg; old_pkg : T_pkg);
   procedure pkg_emit_add_deps_begin    (pkg : T_pkg);
   procedure pkg_emit_add_deps_finished (pkg : T_pkg);
   procedure pkg_emit_extract_begin     (pkg : T_pkg);
   procedure pkg_emit_extract_finished  (pkg : T_pkg);
   procedure pkg_emit_upgrade_begin     (new_pkg : T_pkg; old_pkg : T_pkg);
   procedure pkg_emit_upgrade_finished  (new_pkg : T_pkg; old_pkg : T_pkg);
   procedure pkg_emit_missing_dep       (pkg : T_pkg; dep : T_pkg_dep);
   procedure pkg_emit_locked            (pkg : T_pkg);
   procedure pkg_emit_required          (pkg : T_pkg; force : Boolean);
   procedure pkg_emit_noremotedb        (repo : Text);
   procedure pkg_emit_file_mismatch     (pkg : T_pkg; f : T_pkg_file; newsum : Text);
   procedure pkg_emit_file_missing      (pkg : T_pkg; f : T_pkg_file);
   procedure pkg_emit_progress_start    (msg : Text);
   procedure pkg_emit_progress_tick     (current, total : T_progress_tick);
   procedure pkg_emit_conflicts         (pkg1 : T_pkg; pkg2 : T_pkg; path : Text);

   procedure pkg_emit_backup;
   procedure pkg_emit_restore;
   procedure pkg_emit_nolocaldb;
   procedure pkg_emit_new_action;
   procedure pkg_emit_newpkgversion;
   procedure pkg_emit_integritycheck_begin;
   procedure pkg_emit_delete_files_begin      (pkg : T_pkg);
   procedure pkg_emit_delete_files_finished   (pkg : T_pkg);
   procedure pkg_emit_deinstall_begin         (pkg : T_pkg);
   procedure pkg_emit_deinstall_finished      (pkg : T_pkg);
   procedure pkg_emit_integritycheck_finished (conflicting : Natural);
   procedure pkg_emit_integritycheck_conflict (uid : Text;
                                               path : Text;
                                               conflicts : pkg_event_conflict_crate.Vector);
   procedure pkg_emit_package_not_found       (pkg_name : Text);
   procedure pkg_emit_incremental_update      (reponame : Text; processed : Natural);
   procedure pkg_register_cleanup_callback    (callback : Clean_Callback; callback_data : Text);
   procedure pkg_unregister_cleanup_callback  (callback : Clean_Callback; callback_data : Text);

   function pkg_emit_query_yesno  (deft : Natural; msg : Text) return Boolean;
   function pkg_emit_query_select (msg    : Text;
                                   items  : access pkg_query_items_crate.Vector;
                                   ncount : Natural;
                                   deft   : Natural) return Boolean;

private

   registered_callback      : Event_Callback;
   registered_callback_data : Text;

   --  Event handler with return success
   function pkg_emit_event (event : pkg_event) return Boolean;

   --  Event handler disregarding success
   procedure pkg_emit_event_blind (event : pkg_event);

   --  If configured, send event log through event pipe
   procedure pipe_event (event : pkg_event);

   --  Event category for eventpipe
   function evcat (et : Event_Type) return String;

end Core.Event;
