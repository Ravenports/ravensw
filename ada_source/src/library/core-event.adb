--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;

with Core.Strings;  use Core.Strings;
with Core.Unix;

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
   procedure pkg_emit_incremental_update (reponame : Text; processed : Boolean)
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
   function pkg_emit_query_yesno (deft : Boolean; msg : Text) return Boolean
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
                                   items  : access Text;
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
      function MT (single_quote_template : String) return Text;
      function MT0 (dtype : String) return Text;
      function MT1 (dtype, n1, v1 : String) return Text;
      function MT2 (dtype, n1, v1, n2, v2 : String) return Text;
      function TT (template : Text; token : String; unescaped_value : String) return Text;

      msg  : Text;
      tmpl : Text;
      S1   : constant String := "%1%";
      S2   : constant String := "%2%";
      S3   : constant String := "%3%";
      S4   : constant String := "%4%";

      function MT (single_quote_template : String) return Text
      is
         DQ : String := replace_all (single_quote_template, LAT.Apostrophe, LAT.Quotation);
      begin
         return SUS (json_escape (DQ));
      end MT;

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

      function TT (template : Text; token : String; unescaped_value : String) return Text
      is
         value : String := json_escape (unescaped_value);
      begin
         return replace_substring (template, token, value);
      end TT;
   begin
      if context.eventpipe < 0 then
         return;
      end if;

      case event.event is
         when PKG_EVENT_ERRNO =>
            tmpl := MT2 ("ERROR", "msg", S1 & '(' & S2 & "): " & S3, "errno", S4);
            msg  := TT (TT (TT (TT (tmpl,
                        S1, USS (event.err_function)),
                        S2, USS (event.err_argument)),
                        S3, Unix.strerror (event.err_number)),
                        S4, int2str (event.err_number));
         when PKG_EVENT_ERROR =>
            tmpl := MT1 ("ERROR", "msg", S1);
            msg  := TT (tmpl, S1, USS (event.message));
         when PKG_EVENT_NOTICE =>
            tmpl := MT1 ("NOTICE", "msg", S1);
            msg  := TT (tmpl, S1, USS (event.message));
         when PKG_EVENT_DEVELOPER_MODE =>
            tmpl := MT1 ("ERROR", "msg", "DEVELOPER_MODE: " & S1);
            msg  := TT (tmpl, S1, USS (event.message));
         when PKG_EVENT_UPDATE_ADD =>
            tmpl := MT2 ("INFO_UPDATE_ADD", "fetched", S1, "total", S2);
            msg  := TT (TT (tmpl, S1, int2str (event.done)), S2, int2str (event.total));
         when PKG_EVENT_UPDATE_REMOVE =>
            tmpl := MT2 ("INFO_UPDATE_REMOVE", "fetched", S1, "total", S2);
            msg  := TT (TT (tmpl, S1, int2str (event.done)), S2, int2str (event.total));
         when PKG_EVENT_FETCH_BEGIN =>
            tmpl := MT1 ("INFO_FETCH_BEGIN", "url", S1);
            msg  := TT (tmpl, S1, USS (event.url));
         when PKG_EVENT_FETCH_FINISHED =>
            tmpl := MT1 ("INFO_FETCH_FINISHED", "url", S1);
            msg  := TT (tmpl, S1, USS (event.url));
--         when PKG_EVENT_INSTALL_BEGIN =>
--            tmp1 := MT2 ("INFO_INSTALL_BEGIN", "pkgname", S1, "pkgversion", S2);
--            msg  := TT (TT (tmpl, S1, int2str (event.pk)), S2, int2str (event.total));
         when others => null;
      end case;
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


end Core.Event;
