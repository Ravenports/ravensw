--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;

with Core.Strings;  use Core.Strings;
with Core.Unix;
with Core.Printf;

package body Core.Event is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------
   --  pkg_event_register
   --------------------------------------------------------------------
   procedure pkg_event_register (callback : Event_Callback; callback_data : Text) is
   begin
      registered_callback      := callback;
      registered_callback_data := callback_data;
   end pkg_event_register;


   --------------------------------------------------------------------
   --  pkg_emit_message
   --------------------------------------------------------------------
   procedure pkg_emit_message (message : Text)
   is
      event  : pkg_event (this_event => PKG_EVENT_MESSAGE);
   begin
      event.message := message;
      pkg_emit_event_blind (event);
   end pkg_emit_message;


   --------------------------------------------------------------------
   --  pkg_emit_error
   --------------------------------------------------------------------
   procedure pkg_emit_error   (message : Text)
   is
      event  : pkg_event (this_event => PKG_EVENT_ERROR);
   begin
      event.message := message;
      pkg_emit_event_blind (event);
   end pkg_emit_error;


   --------------------------------------------------------------------
   --  pkg_emit_notice
   --------------------------------------------------------------------
   procedure pkg_emit_notice  (message : Text)
   is
      event  : pkg_event (this_event => PKG_EVENT_NOTICE);
   begin
      event.message := message;
      pkg_emit_event_blind (event);
   end pkg_emit_notice;


   --------------------------------------------------------------------
   --  pkg_emit_developer_mode
   --------------------------------------------------------------------
   procedure pkg_emit_developer_mode (message : Text)
   is
      event  : pkg_event (this_event => PKG_EVENT_DEVELOPER_MODE);
   begin
      event.message := message;
      pkg_emit_event_blind (event);
   end pkg_emit_developer_mode;


   --------------------------------------------------------------------
   --  pkg_emit_errno
   --------------------------------------------------------------------
   procedure pkg_emit_errno (function_name : Text;
                             arguments     : Text;
                             error_number  : Integer)
   is
      event  : pkg_event (this_event => PKG_EVENT_ERRNO);
   begin
      event.err_function := function_name;
      event.err_argument := arguments;
      event.err_number   := error_number;
      pkg_emit_event_blind (event);
   end pkg_emit_errno;


   --------------------------------------------------------------------
   --  pkg_emit_already_installed
   --------------------------------------------------------------------
   procedure pkg_emit_already_installed (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_ALREADY_INSTALLED);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_already_installed;


   --------------------------------------------------------------------
   --  pkg_emit_fetch_begin
   --------------------------------------------------------------------
   procedure pkg_emit_fetch_begin (url : Text)
   is
      event  : pkg_event (this_event => PKG_EVENT_FETCH_BEGIN);
   begin
      event.url := url;
      pkg_emit_event_blind (event);
   end pkg_emit_fetch_begin;


   --------------------------------------------------------------------
   --  pkg_emit_fetch_finished
   --------------------------------------------------------------------
   procedure pkg_emit_fetch_finished (url : Text)
   is
      event  : pkg_event (this_event => PKG_EVENT_FETCH_FINISHED);
   begin
      event.url := url;
      pkg_emit_event_blind (event);
   end pkg_emit_fetch_finished;


   --------------------------------------------------------------------
   --  pkg_emit_update_remove
   --------------------------------------------------------------------
   procedure pkg_emit_update_remove (total, done : Natural)
   is
      event  : pkg_event (this_event => PKG_EVENT_UPDATE_REMOVE);
   begin
      event.total := total;
      event.done  := done;
      pkg_emit_event_blind (event);
   end pkg_emit_update_remove;


   --------------------------------------------------------------------
   --  pkg_emit_update_add
   --------------------------------------------------------------------
   procedure pkg_emit_update_add (total, done : Natural)
   is
      event  : pkg_event (this_event => PKG_EVENT_UPDATE_ADD);
   begin
      event.total := total;
      event.done  := done;
      pkg_emit_event_blind (event);
   end pkg_emit_update_add;


   --------------------------------------------------------------------
   --  pkg_emit_install_begin
   --------------------------------------------------------------------
   procedure pkg_emit_install_begin (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_INSTALL_BEGIN);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_install_begin;


   --------------------------------------------------------------------
   --  pkg_emit_install_finished
   --------------------------------------------------------------------
   procedure pkg_emit_install_finished (new_pkg : T_pkg; old_pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_INSTALL_FINISHED);
   begin
      event.new_pkg := new_pkg;
      event.old_pkg := old_pkg;

      --  TODO:
      --  syslog_enabled = pkg_object_bool(pkg_config_get("SYSLOG"));
      --  if (syslog_enabled) {
      --    syslog(LOG_NOTICE, "%s-%s installed", p->name, p->version);}

      pkg_emit_event_blind (event);
   end pkg_emit_install_finished;


   --------------------------------------------------------------------
   --  pkg_emit_add_deps_begin
   --------------------------------------------------------------------
   procedure pkg_emit_add_deps_begin (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_ADD_DEPS_BEGIN);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_add_deps_begin;


   --------------------------------------------------------------------
   --  pkg_emit_add_deps_finished
   --------------------------------------------------------------------
   procedure pkg_emit_add_deps_finished (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_ADD_DEPS_BEGIN);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_add_deps_finished;


   --------------------------------------------------------------------
   --  pkg_emit_extract_begin
   --------------------------------------------------------------------
   procedure pkg_emit_extract_begin (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_EXTRACT_BEGIN);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_extract_begin;


   --------------------------------------------------------------------
   --  pkg_emit_extract_finished
   --------------------------------------------------------------------
   procedure pkg_emit_extract_finished (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_EXTRACT_FINISHED);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_extract_finished;


   --------------------------------------------------------------------
   --  pkg_emit_delete_files_begin
   --------------------------------------------------------------------
   procedure pkg_emit_delete_files_begin (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_DELETE_FILES_BEGIN);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_delete_files_begin;


   --------------------------------------------------------------------
   --  pkg_emit_delete_files_finished
   --------------------------------------------------------------------
   procedure pkg_emit_delete_files_finished (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_DELETE_FILES_FINISHED);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_delete_files_finished;


   --------------------------------------------------------------------
   --  pkg_emit_integritycheck_begin
   --------------------------------------------------------------------
   procedure pkg_emit_integritycheck_begin
   is
      event  : pkg_event (this_event => PKG_EVENT_INTEGRITYCHECK_BEGIN);
   begin
      pkg_emit_event_blind (event);
   end pkg_emit_integritycheck_begin;


   --------------------------------------------------------------------
   --  pkg_emit_integritycheck_finished
   --------------------------------------------------------------------
   procedure pkg_emit_integritycheck_finished (conflicting : Natural)
   is
      event  : pkg_event (this_event => PKG_EVENT_INTEGRITYCHECK_FINISHED);
   begin
      event.conflicting := conflicting;
      pkg_emit_event_blind (event);
   end pkg_emit_integritycheck_finished;


   --------------------------------------------------------------------
   --  pkg_emit_integritycheck_conflict
   --------------------------------------------------------------------
   procedure pkg_emit_integritycheck_conflict (uid : Text;
                                               path : Text;
                                               conflicts : pkg_event_conflict_crate.Vector)
   is
      event  : pkg_event (this_event => PKG_EVENT_INTEGRITYCHECK_CONFLICT);
   begin
      event.pkg_uid   := uid;
      event.pkg_path  := path;
      event.conflicts := conflicts;
      pkg_emit_event_blind (event);
   end pkg_emit_integritycheck_conflict;


   --------------------------------------------------------------------
   --  pkg_emit_deinstall_begin
   --------------------------------------------------------------------
   procedure pkg_emit_deinstall_begin (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_DEINSTALL_BEGIN);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_deinstall_begin;


   --------------------------------------------------------------------
   --  pkg_emit_deinstall_finished
   --------------------------------------------------------------------
   procedure pkg_emit_deinstall_finished (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_DEINSTALL_FINISHED);
   begin
      event.pkg := pkg;

      --  TODO:
      --  syslog_enabled = pkg_object_bool(pkg_config_get("SYSLOG"));
      --  if (syslog_enabled) {
      --    syslog(LOG_NOTICE, "%s-%s deinstalled", p->name, p->version);}

      pkg_emit_event_blind (event);
   end pkg_emit_deinstall_finished;


   --------------------------------------------------------------------
   --  pkg_emit_upgrade_begin
   --------------------------------------------------------------------
   procedure pkg_emit_upgrade_begin (new_pkg : T_pkg; old_pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_UPGRADE_BEGIN);
   begin
      event.new_pkg := new_pkg;
      event.old_pkg := old_pkg;

      pkg_emit_event_blind (event);
   end pkg_emit_upgrade_begin;


   --------------------------------------------------------------------
   --  pkg_emit_upgrade_finished
   --------------------------------------------------------------------
   procedure pkg_emit_upgrade_finished  (new_pkg : T_pkg; old_pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_UPGRADE_FINISHED);
   begin
      event.new_pkg := new_pkg;
      event.old_pkg := old_pkg;

--  TODO:
--          syslog_enabled = pkg_object_bool(pkg_config_get("SYSLOG"));
--          if (syslog_enabled) {
--              const char *actions[] = {
--                   [PKG_DOWNGRADE] = "downgraded",
--                   [PKG_REINSTALL] = "reinstalled",
--                   [PKG_UPGRADE]   = "upgraded",
--          };
--          pkg_change_t action;
--
--          action = pkg_version_change_between(new, old);
--              syslog(LOG_NOTICE, "%s %s: %s %s %s ",
--                   new->name, actions[action],
--                   old->version != NULL ? old->version : new->version,
--                   old->version != NULL ? "->" : "",
--                   old->version != NULL ? new->version : "");
--          }

      pkg_emit_event_blind (event);
   end pkg_emit_upgrade_finished;


   --------------------------------------------------------------------
   --  pkg_emit_missing_dep
   --------------------------------------------------------------------
   procedure pkg_emit_missing_dep (pkg : T_pkg; dep : T_pkg_dep)
   is
      event  : pkg_event (this_event => PKG_EVENT_MISSING_DEP);
   begin
      event.dep_pkg := pkg;
      event.dep_dep := dep;

      pkg_emit_event_blind (event);
   end pkg_emit_missing_dep;


   --------------------------------------------------------------------
   --  pkg_emit_locked
   --------------------------------------------------------------------
   procedure pkg_emit_locked (pkg : T_pkg)
   is
      event  : pkg_event (this_event => PKG_EVENT_LOCKED);
   begin
      event.pkg := pkg;
      pkg_emit_event_blind (event);
   end pkg_emit_locked;


   --------------------------------------------------------------------
   --  pkg_emit_required
   --------------------------------------------------------------------
   procedure pkg_emit_required (pkg : T_pkg; force : Boolean)
   is
      event  : pkg_event (this_event => PKG_EVENT_REQUIRED);
   begin
      event.req_pkg := pkg;
      event.force   := force;
      pkg_emit_event_blind (event);
   end pkg_emit_required;


   --------------------------------------------------------------------
   --  pkg_emit_nolocaldb
   --------------------------------------------------------------------
   procedure pkg_emit_nolocaldb
   is
      event  : pkg_event (this_event => PKG_EVENT_NOLOCALDB);
   begin
      pkg_emit_event_blind (event);
   end pkg_emit_nolocaldb;


   --------------------------------------------------------------------
   --  pkg_emit_newpkgversion
   --------------------------------------------------------------------
   procedure pkg_emit_newpkgversion
   is
      event  : pkg_event (this_event => PKG_EVENT_NEWPKGVERSION);
   begin
      pkg_emit_event_blind (event);
   end pkg_emit_newpkgversion;


   --------------------------------------------------------------------
   --  pkg_emit_noremotedb
   --------------------------------------------------------------------
   procedure pkg_emit_noremotedb (repo : Text)
   is
      event  : pkg_event (this_event => PKG_EVENT_NOREMOTEDB);
   begin
      event.repo := repo;
      pkg_emit_event_blind (event);
   end pkg_emit_noremotedb;


   --------------------------------------------------------------------
   --  pkg_emit_file_mismatch
   --------------------------------------------------------------------
   procedure pkg_emit_file_mismatch (pkg : T_pkg; f : T_pkg_file; newsum : Text)
   is
      event  : pkg_event (this_event => PKG_EVENT_FILE_MISMATCH);
   begin
      event.fmm_pkg  := pkg;
      event.fmm_file := f;
      event.newsum   := newsum;
      pkg_emit_event_blind (event);
   end pkg_emit_file_mismatch;


   --------------------------------------------------------------------
   --  pkg_emit_file_missing
   --------------------------------------------------------------------
   procedure pkg_emit_file_missing (pkg : T_pkg; f : T_pkg_file)
   is
      event  : pkg_event (this_event => PKG_EVENT_FILE_MISSING);
   begin
      event.miss_pkg  := pkg;
      event.miss_file := f;
      pkg_emit_event_blind (event);
   end pkg_emit_file_missing;


   --------------------------------------------------------------------
   --  pkg_emit_package_not_found
   --------------------------------------------------------------------
   procedure pkg_emit_package_not_found (pkg_name : Text)
   is
      event : pkg_event (this_event => PKG_EVENT_NOT_FOUND);
   begin
      event.pkg_name := pkg_name;
      pkg_emit_event_blind (event);
   end pkg_emit_package_not_found;


   --------------------------------------------------------------------
   --  pkg_emit_incremental_update
   --------------------------------------------------------------------
   procedure pkg_emit_incremental_update (reponame : Text; processed : Natural)
   is
      event : pkg_event (this_event => PKG_EVENT_INCREMENTAL_UPDATE);
   begin
      event.reponame := reponame;
      event.processed := processed;
      pkg_emit_event_blind (event);
   end pkg_emit_incremental_update;


   --------------------------------------------------------------------
   --  pkg_emit_query_yesno
   --------------------------------------------------------------------
   function pkg_emit_query_yesno (deft : Natural; msg : Text) return Boolean
   is
      event : pkg_event (this_event => PKG_EVENT_QUERY_YESNO);
      result : Boolean;
   begin
      event.qyesno_msg  := msg;
      event.qyesno_deft := deft;
      result := pkg_emit_event (event);
      return result;
   end pkg_emit_query_yesno;


   --------------------------------------------------------------------
   --  pkg_emit_query_select
   --------------------------------------------------------------------
   function pkg_emit_query_select (msg    : Text;
                                   items  : access pkg_query_items_crate.Vector;
                                   ncount : Natural;
                                   deft   : Natural) return Boolean
   is
      event : pkg_event (this_event => PKG_EVENT_QUERY_SELECT);
      result : Boolean;
   begin
      event.query_msg    := msg;
      event.query_items  := items;
      event.query_ncount := ncount;
      event.query_deft   := deft;

      result := pkg_emit_event (event);
      return result;
   end pkg_emit_query_select;


   --------------------------------------------------------------------
   --  pkg_emit_backup
   --------------------------------------------------------------------
   procedure pkg_emit_backup
   is
      event  : pkg_event (this_event => PKG_EVENT_BACKUP);
   begin
      pkg_emit_event_blind (event);
   end pkg_emit_backup;


   --------------------------------------------------------------------
   --  pkg_emit_restore
   --------------------------------------------------------------------
   procedure pkg_emit_restore
   is
      event  : pkg_event (this_event => PKG_EVENT_RESTORE);
   begin
      pkg_emit_event_blind (event);
   end pkg_emit_restore;


   --------------------------------------------------------------------
   --  pkg_emit_progress_start
   --------------------------------------------------------------------
   procedure pkg_emit_progress_start (msg : Text)
   is
      event : pkg_event (this_event => PKG_EVENT_PROGRESS_START);
   begin
      event.message := msg;
      pkg_emit_event_blind (event);
   end pkg_emit_progress_start;


   --------------------------------------------------------------------
   --  pkg_emit_progress_tick
   --------------------------------------------------------------------
   procedure pkg_emit_progress_tick (current, total : T_progress_tick)
   is
      event : pkg_event (this_event => PKG_EVENT_PROGRESS_TICK);
   begin
      event.prog_current := current;
      event.prog_total   := total;
      pkg_emit_event_blind (event);
   end pkg_emit_progress_tick;


   --------------------------------------------------------------------
   --  pkg_emit_new_action
   --------------------------------------------------------------------
   procedure pkg_emit_new_action
   is
      event : pkg_event (this_event => PKG_EVENT_NEW_ACTION);
   begin
      pkg_emit_event_blind (event);
   end pkg_emit_new_action;


   --------------------------------------------------------------------
   --  pkg_emit_conflicts
   --------------------------------------------------------------------
   procedure pkg_emit_conflicts (pkg1 : T_pkg; pkg2 : T_pkg; path : Text)
   is
      event : pkg_event (this_event => PKG_EVENT_CONFLICTS);
   begin
      event.conflict_pkg1 := pkg1;
      event.conflict_pkg2 := pkg2;
      event.conflict_path := path;
      pkg_emit_event_blind (event);
   end pkg_emit_conflicts;


   --------------------------------------------------------------------
   --  pkg_register_cleanup_callback
   --------------------------------------------------------------------
   procedure pkg_register_cleanup_callback (callback : Clean_Callback; callback_data : Text)
   is
      event : pkg_event (this_event => PKG_EVENT_CLEANUP_CALLBACK_REGISTER);
   begin
      event.cleanup_callback      := callback;
      event.cleanup_callback_data := callback_data;
      pkg_emit_event_blind (event);
   end pkg_register_cleanup_callback;


   --------------------------------------------------------------------
   --  pkg_unregister_cleanup_callback
   --------------------------------------------------------------------
   procedure pkg_unregister_cleanup_callback (callback : Clean_Callback; callback_data : Text)
   is
      event : pkg_event (this_event => PKG_EVENT_CLEANUP_CALLBACK_UNREGISTER);
   begin
      event.cleanup_callback      := callback;
      event.cleanup_callback_data := callback_data;
      pkg_emit_event_blind (event);
   end pkg_unregister_cleanup_callback;


   --------------------------------------------------------------------
   --  pipe_event
   --------------------------------------------------------------------
   procedure pipe_event (event : pkg_event)
   is
      function MT0 (dtype : String) return Text;
      function MT1 (dtype, n1, v1 : String) return Text;
      function MT2 (dtype, n1, v1, n2, v2 : String) return Text;
      function MT3 (dtype, n1, v1, n2, v2, n3, v3 : String) return Text;
      function MC3 (dtype, n1, v1, n2, v2, n3, v3 : String) return Text;
      function MC4 (dtype, n1, v1, n2, v2, n3, v3, n4, v4 : String) return Text;
      function TT (template : Text; token : String; unescaped_value : String) return Text;

      msg  : Text;
      tmpl : Text;
      S1   : constant String := "%1%";
      S2   : constant String := "%2%";
      S3   : constant String := "%3%";
      S4   : constant String := "%4%";
      cat  : constant String := evcat (event.event);


      function MT0 (dtype : String) return Text
      is
         SQ : String := "{'type': '" & dtype & "', 'data': {}}";
         DQ : String := replace_all (SQ, LAT.Apostrophe, LAT.Quotation);
      begin
         return SUS (json_escape (DQ));
      end MT0;

      function MT1 (dtype, n1, v1 : String) return Text
      is
         SQ : String := "{'type': '" & dtype & "', 'data': {'" & n1 & "': '" & v1 & "'}}";
         DQ : String := replace_all (SQ, LAT.Apostrophe, LAT.Quotation);
      begin
         return SUS (json_escape (DQ));
      end MT1;

      function MT2 (dtype, n1, v1, n2, v2 : String) return Text
      is
         SQ : String := "{'type': '" & dtype & "', 'data': {'" & n1 & "': '" & v1 &
           "', '" & n2 & "': '" & v2 & "'}}";
         DQ : String := replace_all (SQ, LAT.Apostrophe, LAT.Quotation);
      begin
         return SUS (json_escape (DQ));
      end MT2;

      function MT3 (dtype, n1, v1, n2, v2, n3, v3 : String) return Text
      is
         SQ : String := "{'type': '" & dtype & "', 'data': {'" & n1 & "': '" & v1 &
           "', '" & n2 & "': '" & v2 & "', '" & n3 & "': '" & v3 & "'}}";
         DQ : String := replace_all (SQ, LAT.Apostrophe, LAT.Quotation);
      begin
         return SUS (json_escape (DQ));
      end MT3;

      function MC3 (dtype, n1, v1, n2, v2, n3, v3 : String) return Text
      is
         SQ : String := "{'type': '" & dtype & "', 'data': {'" & n1 & "': '" & v1 &
           "', '" & n2 & "': '" & v2 & "', '" & n3 & "': [" & v3 & "]}}";
         DQ : String := replace_all (SQ, LAT.Apostrophe, LAT.Quotation);
      begin
         return SUS (json_escape (DQ));
      end MC3;

      function MC4 (dtype, n1, v1, n2, v2, n3, v3, n4, v4 : String) return Text
      is
         SQ : String := "{'type': '" & dtype & "', 'data': {'" & n1 & "': '" & v1 &
           "', '" & n2 & "': '" & v2 & "', '" & n3 & "': '" & v3 & "', '"
           & n4 & "': [" & v4 & "]}}";
         DQ : String := replace_all (SQ, LAT.Apostrophe, LAT.Quotation);
      begin
         return SUS (json_escape (DQ));
      end MC4;

      function TT (template : Text; token : String; unescaped_value : String) return Text
      is
         value : String := json_escape (unescaped_value);
      begin
         return replace_substring (template, token, value);
      end TT;

      use type Unix.Unix_File_Descriptor;
   begin
      if context.eventpipe = Unix.not_connected then
         return;
      end if;

      case event.event is
         when PKG_EVENT_DEBUG |
              PKG_EVENT_BACKUP |
              PKG_EVENT_RESTORE |
              PKG_EVENT_FETCHING |
              PKG_EVENT_ARCHIVE_COMP_UNSUP |
              PKG_EVENT_DELETE_FILES_BEGIN |
              PKG_EVENT_DELETE_FILES_FINISHED |
              PKG_EVENT_ADD_DEPS_BEGIN |
              PKG_EVENT_ADD_DEPS_FINISHED |
              PKG_EVENT_CLEANUP_CALLBACK_REGISTER |
              PKG_EVENT_CLEANUP_CALLBACK_UNREGISTER |
              PKG_EVENT_FAILED_CKSUM |
              PKG_EVENT_CREATE_DB_ERROR |
              PKG_EVENT_NOT_FOUND |
              PKG_EVENT_NEW_ACTION |
              PKG_EVENT_MESSAGE |
              PKG_EVENT_FILE_MISSING |
              PKG_EVENT_CONFLICTS =>
            null;

         when PKG_EVENT_ERRNO =>
            tmpl := MT2 ("ERROR", "msg", S1 & '(' & S2 & "): " & S3, "errno", S4);
            msg  := TT (TT (TT (TT (tmpl,
                        S1, USS (event.err_function)),
                        S2, USS (event.err_argument)),
                        S3, Unix.strerror (event.err_number)),
                        S4, int2str (event.err_number));

         when PKG_EVENT_ERROR |
            PKG_EVENT_NOTICE =>
            tmpl := MT1 (cat, "msg", S1);
            msg  := TT (tmpl, S1, USS (event.message));

         when PKG_EVENT_DEVELOPER_MODE =>
            tmpl := MT1 (cat, "msg", "DEVELOPER_MODE: " & S1);
            msg  := TT (tmpl, S1, USS (event.message));

         when PKG_EVENT_UPDATE_ADD |
            PKG_EVENT_UPDATE_REMOVE =>
            tmpl := MT2 (cat, "fetched", S1, "total", S2);
            msg  := TT (TT (tmpl, S1, int2str (event.done)), S2, int2str (event.total));

         when PKG_EVENT_FETCH_BEGIN |
              PKG_EVENT_FETCH_FINISHED =>
            tmpl := MT1 (cat, "url", S1);
            msg  := TT (tmpl, S1, USS (event.url));

         when PKG_EVENT_INSTALL_BEGIN |
              PKG_EVENT_EXTRACT_BEGIN |
              PKG_EVENT_EXTRACT_FINISHED |
              PKG_EVENT_DEINSTALL_BEGIN |
              PKG_EVENT_DEINSTALL_FINISHED |
              PKG_EVENT_ALREADY_INSTALLED |
              PKG_EVENT_LOCKED =>
            tmpl := MT2 (cat, "pkgname", S1, "pkgversion", S2);
            msg  := TT (TT (tmpl,
                        S1, Printf.format_name (event.pkg)),
                        S2, Printf.format_version (event.pkg));

         when PKG_EVENT_INSTALL_FINISHED =>
            tmpl := MT3 (cat, "pkgname", S1, "pkgversion", S2, "message", S3);
            msg := TT (TT (TT (tmpl,
                       S1, Printf.format_name (event.new_pkg)),
                       S2, Printf.format_version (event.new_pkg)),
                       S3, Printf.first_message_contents (event.new_pkg));

         when PKG_EVENT_INTEGRITYCHECK_BEGIN |
              PKG_EVENT_NOLOCALDB |
              PKG_EVENT_NEWPKGVERSION |
              PKG_EVENT_PROGRESS_START =>
            msg := MT0 (cat);

         when PKG_EVENT_INTEGRITYCHECK_FINISHED =>
            tmpl := MT1 (cat, "conflicting", S1);
            msg  := TT (tmpl, S1, int2str (event.conflicting));

         when PKG_EVENT_UPGRADE_BEGIN |
            PKG_EVENT_UPGRADE_FINISHED =>
            tmpl := MT3 (cat, "pkgname", S1, "pkgversion", S2, "pkgnewversion", S3);
            msg := TT (TT (TT (tmpl,
                       S1, Printf.format_name (event.old_pkg)),
                       S2, Printf.format_version (event.old_pkg)),
                       S3, Printf.format_name (event.new_pkg));

         when PKG_EVENT_MISSING_DEP =>
            tmpl := MT2 (cat, "depname", S1, "depversion", S2);
            msg  := TT (TT (tmpl,
                        S1, USS (event.dep_dep.name)),
                        S2, USS (event.dep_dep.version));

         when PKG_EVENT_NOREMOTEDB =>
            tmpl := MT1 (cat, "url", S1);
            msg  := TT (tmpl, S1, USS (event.repo));

         when PKG_EVENT_FILE_MISMATCH =>
            tmpl := MT3 (cat, "pkgname", S1, "pkgversion", S2, "path", S3);
            msg := TT (TT (TT (tmpl,
                       S1, Printf.format_name (event.fmm_pkg)),
                       S2, Printf.format_version (event.fmm_pkg)),
                       S3, USS (event.fmm_file.path));

         when PKG_EVENT_INCREMENTAL_UPDATE =>
            tmpl := MT2 (cat, "name", S1, "processed", S2);
            msg  := TT (TT (tmpl,
                        S1, USS (event.reponame)),
                        S2, int2str (event.processed));

         when PKG_EVENT_QUERY_YESNO =>
            tmpl := MT2 (cat, "msg", S1, "default", S2);
            msg  := TT (TT (tmpl,
                        S1, USS (event.qyesno_msg)),
                        S2, int2str (event.qyesno_deft));

         when PKG_EVENT_PROGRESS_TICK =>
            tmpl := MT2 (cat, "current", S1, "total", S2);
            msg  := TT (TT (tmpl,
                        S1, int2str (Integer (event.prog_current))),
                        S2, int2str (Integer (event.prog_total)));

         when PKG_EVENT_INTEGRITYCHECK_CONFLICT =>
            tmpl := MC3 (cat, "pkguid", S1, "pkgpath", S2, "conflicts", S3);
            declare
               procedure check_conflict (position : pkg_event_conflict_crate.Cursor);

               conflicts : Text;

               procedure check_conflict (position : pkg_event_conflict_crate.Cursor)
               is
                  clash : constant String := USS (pkg_event_conflict_crate.Element (position));
               begin
                  if not IsBlank (conflicts) then
                     SU.Append (conflicts, ", ");
                  end if;
                  SU.Append (conflicts, LAT.Left_Curly_Bracket &
                               LAT.Quotation & "uid" & LAT.Quotation & ": " &
                               LAT.Quotation & clash & LAT.Quotation &
                               LAT.Right_Curly_Bracket);
               end check_conflict;

            begin
               event.conflicts.Iterate (check_conflict'Access);
               msg := TT (TT (TT (tmpl,
                       S1, USS (event.pkg_uid)),
                       S2, USS (event.pkg_path)),
                       S3, USS (conflicts));
            end;

         when PKG_EVENT_REQUIRED =>
            tmpl := MC4 (cat, "pkgname", S1, "pkgversion", S2, "force", S3, "required_by", S4);
            declare
               function force_arg (force : Boolean) return String;
               procedure print_depends (position : depends_crate.Cursor);

               reqby : Text;

               function force_arg (force : Boolean) return String is
               begin
                  if force then
                     return "true";
                  else
                     return "false";
                  end if;
               end force_arg;

               procedure print_depends (position : depends_crate.Cursor)
               is
                  dep : T_pkg_dep renames depends_crate.Element (position);
               begin
                  if not IsBlank (reqby) then
                     SU.Append (reqby, ", ");
                  end if;
                  SU.Append (reqby, LAT.Left_Curly_Bracket &
                               LAT.Quotation & "pkgname" & LAT.Quotation & ": " &
                               LAT.Quotation & USS (dep.name) & LAT.Quotation & ", " &
                               LAT.Quotation & "pkgversion" & LAT.Quotation & ": " &
                               LAT.Quotation & USS (dep.version) & LAT.Quotation &
                               LAT.Right_Curly_Bracket);
               end print_depends;
            begin
               event.req_pkg.rdepends.Iterate (print_depends'Access);

               msg := TT (TT (TT (TT (tmpl,
                          S1, Printf.format_name (event.req_pkg)),
                          S2, Printf.format_version (event.req_pkg)),
                          S3, force_arg (event.force)),
                          S4, USS (reqby));
            end;

         when PKG_EVENT_QUERY_SELECT =>
            tmpl := MC4 (cat, "msg", S1, "ncnt", S2, "default", S3, "items", S4);
            declare
               procedure print_items (position : pkg_query_items_crate.Cursor);

               array_contents : Text;

               procedure print_items (position : pkg_query_items_crate.Cursor)
               is
                  item : constant String := USS (pkg_query_items_crate.Element (position));
               begin
                  if not IsBlank (array_contents) then
                     SU.Append (array_contents, ", ");
                  end if;
                  SU.Append (array_contents, LAT.Left_Curly_Bracket &
                               LAT.Quotation & "text" & LAT.Quotation & ": " &
                               LAT.Quotation & item & LAT.Quotation &
                               LAT.Right_Curly_Bracket);
               end print_items;
            begin
               event.query_items.all.Iterate (print_items'Access);

               msg := TT (TT (TT (TT (tmpl,
                          S1, USS (event.query_msg)),
                          S2, int2str (event.query_ncount)),
                          S3, int2str (event.query_deft)),
                          S4, USS (array_contents));
            end;
      end case;

      --  TODO: print to pipe here
   end pipe_event;


   --------------------------------------------------------------------
   --  pkg_emit_event
   --------------------------------------------------------------------
   function pkg_emit_event (event : pkg_event) return Boolean
   is
      result : Boolean := False;
   begin
      if registered_callback /= null then
         result := registered_callback (event, registered_callback_data);
      end if;
      pipe_event (event);
      return result;
   end pkg_emit_event;


   --------------------------------------------------------------------
   --  pkg_emit_event_blind
   --------------------------------------------------------------------
   procedure pkg_emit_event_blind (event : pkg_event)
   is
      result : Boolean := False;
   begin
      if registered_callback /= null then
         result := registered_callback (event, registered_callback_data);
      end if;
      pipe_event (event);
   end pkg_emit_event_blind;


   --------------------------------------------------------------------
   --  evcat
   --------------------------------------------------------------------
   function evcat (et : Event_Type) return String is
   begin
      case et is
         when PKG_EVENT_ERRNO |
              PKG_EVENT_ERROR |
              PKG_EVENT_DEVELOPER_MODE          => return "ERROR";
         when PKG_EVENT_NOTICE                  => return "NOTICE";
         when PKG_EVENT_UPDATE_ADD              => return "INFO_UPDATE_ADD";
         when PKG_EVENT_UPDATE_REMOVE           => return "INFO_UPDATE_REMOVE";
         when PKG_EVENT_FETCH_BEGIN             => return "INFO_FETCH_BEGIN";
         when PKG_EVENT_FETCH_FINISHED          => return "INFO_FETCH_FINISHED";
         when PKG_EVENT_INSTALL_BEGIN           => return "INFO_INSTALL_BEGIN";
         when PKG_EVENT_INSTALL_FINISHED        => return "INFO_INSTALL_FINISHED";
         when PKG_EVENT_EXTRACT_BEGIN           => return "INFO_EXTRACT_BEGIN";
         when PKG_EVENT_EXTRACT_FINISHED        => return "INFO_EXTRACT_FINISHED";
         when PKG_EVENT_INTEGRITYCHECK_BEGIN    => return "INFO_INTEGRITYCHECK_BEGIN";
         when PKG_EVENT_INTEGRITYCHECK_CONFLICT => return "INFO_INTEGRITYCHECK_CONFLICT";
         when PKG_EVENT_INTEGRITYCHECK_FINISHED => return "INFO_INTEGRITYCHECK_FINISHED";
         when PKG_EVENT_DEINSTALL_BEGIN         => return "INFO_DEINSTALL_BEGIN";
         when PKG_EVENT_DEINSTALL_FINISHED      => return "INFO_DEINSTALL_FINISHED";
         when PKG_EVENT_UPGRADE_BEGIN           => return "INFO_UPGRADE_BEGIN";
         when PKG_EVENT_UPGRADE_FINISHED        => return "INFO_UPGRADE_FINISHED";
         when PKG_EVENT_LOCKED                  => return "ERROR_LOCKED";
         when PKG_EVENT_REQUIRED                => return "ERROR_REQUIRED";
         when PKG_EVENT_ALREADY_INSTALLED       => return "ERROR_ALREADY_INSTALLED";
         when PKG_EVENT_MISSING_DEP             => return "ERROR_MISSING_DEP";
         when PKG_EVENT_NOREMOTEDB              => return "ERROR_NOREMOTEDB";
         when PKG_EVENT_NOLOCALDB               => return "ERROR_NOLOCALDB";
         when PKG_EVENT_NEWPKGVERSION           => return "INFO_NEWPKGVERSION";
         when PKG_EVENT_FILE_MISMATCH           => return "ERROR_FILE_MISMATCH";
         when PKG_EVENT_INCREMENTAL_UPDATE      => return "INFO_INCREMENTAL_UPDATE";
         when PKG_EVENT_QUERY_YESNO             => return "QUERY_YESNO";
         when PKG_EVENT_QUERY_SELECT            => return "QUERY_SELECT";
         when PKG_EVENT_PROGRESS_START          => return "INFO_PROGRESS_START";
         when PKG_EVENT_PROGRESS_TICK           => return "PKG_EVENT_PROGRESS_TICK";
         when PKG_EVENT_DEBUG |
              PKG_EVENT_BACKUP |
              PKG_EVENT_RESTORE |
              PKG_EVENT_FETCHING |
              PKG_EVENT_ARCHIVE_COMP_UNSUP |
              PKG_EVENT_DELETE_FILES_BEGIN |
              PKG_EVENT_DELETE_FILES_FINISHED |
              PKG_EVENT_ADD_DEPS_BEGIN |
              PKG_EVENT_ADD_DEPS_FINISHED |
              PKG_EVENT_CLEANUP_CALLBACK_REGISTER |
              PKG_EVENT_CLEANUP_CALLBACK_UNREGISTER |
              PKG_EVENT_FAILED_CKSUM |
              PKG_EVENT_CREATE_DB_ERROR |
              PKG_EVENT_NOT_FOUND |
              PKG_EVENT_NEW_ACTION |
              PKG_EVENT_MESSAGE |
              PKG_EVENT_FILE_MISSING |
              PKG_EVENT_CONFLICTS               => return "unused";
      end case;
   end evcat;


end Core.Event;
