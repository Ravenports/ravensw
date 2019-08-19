--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Unix;
with Core.Config;
with Core.Pkg;      use Core.Pkg;
with Core.Strings;  use Core.Strings;
with Core.Printf;

package body Cmd.Event is

   package PRT renames Core.Printf;

   --------------------------------------------------------------------
   --  warn
   --------------------------------------------------------------------
   procedure warn  (message : String; error : Integer) is
      suffix : String := ": " & Unix.strerror (error);
   begin
      TIO.Put_Line (TIO.Standard_Error, message & suffix);
   end warn;

   --------------------------------------------------------------------
   --  warnx
   --------------------------------------------------------------------
   procedure warnx (verbatim_message : String) is
   begin
      TIO.Put_Line (TIO.Standard_Error, verbatim_message);
   end warnx;


   --------------------------------------------------------------------
   --  handle_event_callbacks
   --------------------------------------------------------------------
   procedure handle_event_callbacks (comline : Cldata)
   is
   begin
      if comline.verb_quiet then
         event_mute := True;
      end if;
      event_debug := comline.glob_debug;
      EV.pkg_event_register (callback      => event_callback'Access,
                             callback_data => blank);
   end handle_event_callbacks;


   --------------------------------------------------------------------
   --  event_callback
   --------------------------------------------------------------------
   function event_callback (event : EV.pkg_event; data : Text) return Boolean
   is
   begin
      case event.this_event is

         when EV.PKG_EVENT_ERRNO =>
            warn (USS (event.err_function) & '(' & USS (event.err_argument) & ')',
                  event.err_number);

         when EV.PKG_EVENT_ERROR =>
            warnx (USS (event.message));

         when EV.PKG_EVENT_NOTICE =>
            if not event_mute then
               TIO.Put_Line (USS (event.message));
            end if;

         when EV.PKG_EVENT_DEVELOPER_MODE =>
            warnx ("DEVELOPER_MODE: " & USS (event.message));

         when EV.PKG_EVENT_UPDATE_ADD =>
            if not event_mute then
               null;  -- TODO (isatty too?)
            end if;

         when EV.PKG_EVENT_UPDATE_REMOVE =>
            if not event_mute then
               null;  -- TODO (isatty too?)
            end if;

         when EV.PKG_EVENT_FETCH_BEGIN =>
            if not event_mute then
               null;  -- TODO
            end if;

         when EV.PKG_EVENT_FETCH_FINISHED =>
            progress_debit := False;

         when EV.PKG_EVENT_INSTALL_BEGIN =>
            null;  -- TODO

         when EV.PKG_EVENT_EXTRACT_BEGIN =>
            if not event_mute then
               null;  -- TODO
            end if;

         when EV.PKG_EVENT_ADD_DEPS_BEGIN =>
            add_deps_depth := add_deps_depth + 1;

         when EV.PKG_EVENT_ADD_DEPS_FINISHED =>
            add_deps_depth := add_deps_depth - 1;

         when EV.PKG_EVENT_INTEGRITYCHECK_BEGIN =>
            if not event_mute then
               TIO.Put ("Checking integrity...");
            end if;

         when EV.PKG_EVENT_INTEGRITYCHECK_FINISHED =>
            if not event_mute then
               declare
                  procedure list (position : pkg_event_conflict_crate.Cursor);
                  procedure list (position : pkg_event_conflict_crate.Cursor)
                  is
                     clash : constant String := USS (pkg_event_conflict_crate.Element (position));
                  begin
                     TIO.Put_Line (clash);
                  end list;
               begin
                  TIO.Put_Line
                    (" done (" & int2str (event.conflicting) & " conflicting)");
                  event.conflicts.Iterate (list'Access);
               end;
            end if;

         when EV.PKG_EVENT_INTEGRITYCHECK_CONFLICT =>
            if event_debug > 0 then
               TIO.Put_Line ("");
               TIO.Put ("Conflict found on path " & USS (event.pkg_path) &
                          " between " & USS (event.pkg_uid) & " and ");
               declare
                  virgin : Boolean := True;
                  procedure list (position : pkg_event_conflict_crate.Cursor);
                  procedure list (position : pkg_event_conflict_crate.Cursor)
                  is
                     clash : constant String := USS (pkg_event_conflict_crate.Element (position));
                  begin
                     if virgin then
                        virgin := False;
                     else
                        TIO.Put (", ");
                     end if;
                  end list;
               begin
                  event.conflicts.Iterate (list'Access);
                  TIO.Put_Line ("");
               end;
            end if;

         when EV.PKG_EVENT_DEINSTALL_BEGIN =>
            if not event_mute then
               null;  --  TODO
            end if;

         when EV.PKG_EVENT_DELETE_FILES_BEGIN =>
            if not event_mute then
               null;  --  TODO
            end if;

         when EV.PKG_EVENT_UPGRADE_BEGIN =>
            if not event_mute then
               null;  --  TODO
            end if;

         when EV.PKG_EVENT_LOCKED =>
            TIO.Put_Line ("");
            TIO.Put_Line (PRT.format_name (event.pkg) & "-" & PRT.format_version (event.pkg) &
                            "is locked and may not be modified");

         when EV.PKG_EVENT_REQUIRED =>
            TIO.Put_Line (TIO.Standard_Error, "");
            TIO.Put (TIO.Standard_Error,
                     PRT.format_name (event.req_pkg) & "-" &
                       PRT.format_version (event.req_pkg) &
                       " is required by: "); --  TODO: %r%{%rn-%rv%| %}", pkg, pkg, pkg);
            if event.force then
               TIO.Put_Line (TIO.Standard_Error, ", deleting anyway");
            else
               TIO.Put_Line (TIO.Standard_Error, "");
            end if;

         when EV.PKG_EVENT_ALREADY_INSTALLED =>
            if not event_mute then
               warnx ("The most recent version of " &
                        PRT.format_name (event.pkg) & "-" & PRT.format_version (event.pkg) &
                        " is already installed");
            end if;

         when EV.PKG_EVENT_NOT_FOUND =>
            warnx ("Package '" & USS (event.pkg_name) &
                     "' was not found in the repositories");

         when EV.PKG_EVENT_MISSING_DEP =>
            warnx ("Missing dependency '" & USS (event.dep_pkg.name) & "'");

         when EV.PKG_EVENT_NOREMOTEDB =>
            warnx ("Unable to open remote database '" & USS (event.repo) & "'. " &
                     "Try running '" & progname & " update' first.");

         when EV.PKG_EVENT_NOLOCALDB =>
            warnx ("Local package database nonexistent!");

         when EV.PKG_EVENT_NEWPKGVERSION =>
            newpkgversion := True;
            TIO.Put_Line ("New version of " & progname &
                            " detected; it needs to be installed first.");

         when EV.PKG_EVENT_FILE_MISMATCH =>
            warnx (PRT.format_name (event.fmm_pkg) & "-" &
                     PRT.format_version (event.fmm_pkg) &
                     ": checksum mismatch for " & USS (event.fmm_file.path));

         when EV.PKG_EVENT_FILE_MISSING =>
            warnx (PRT.format_name (event.miss_pkg) & "-" &
                     PRT.format_version (event.miss_pkg) &
                     ": missing file " & USS (event.miss_file.path));

         when EV.PKG_EVENT_INCREMENTAL_UPDATE =>
            if not event_mute then
               TIO.Put_Line (USS (event.reponame) & " repository update completed. " &
                               int2str (event.processed) & " packages processed.");
            end if;

         when EV.PKG_EVENT_DEBUG =>
            warnx ("DBG(" & int2str (event.debug_level) & ")[" &
                     int2str (Integer (Unix.getpid)) & "]> " & USS (event.debug_msg));

         when EV.PKG_EVENT_QUERY_YESNO =>
            if event.qyesno_deft > 0 then
               null;  --  TODO: return query_yesno(true, ev->e_query_yesno.msg, "[Y/n]") :
            else
               null;  --  TODO: return query_yesno(false, ev->e_query_yesno.msg, "[y/N]") );
            end if;

         when EV.PKG_EVENT_QUERY_SELECT =>
            null;   --  TODO: return query_select(ev->e_query_select.msg, ev->e_query_select.items,

         when EV.PKG_EVENT_PROGRESS_START =>
            null;    -- TODO: progressbar_start(ev->e_progress_start.msg);

         when EV.PKG_EVENT_PROGRESS_TICK =>
            null;    --  TODO: progressbar_tick(ev->e_progress_tick.current,
                     --  ev->e_progress_tick.total);

         when EV.PKG_EVENT_BACKUP =>
            message_buffer := SUS ("Backing up");

         when EV.PKG_EVENT_RESTORE =>
            message_buffer := SUS ("Restoring");

         when EV.PKG_EVENT_NEW_ACTION =>
            nbdone := nbdone + 1;

         when EV.PKG_EVENT_MESSAGE =>
            TIO.Put_Line (USS (event.message));

         when EV.PKG_EVENT_CLEANUP_CALLBACK_REGISTER =>
            null;  --  TODO

         when EV.PKG_EVENT_CLEANUP_CALLBACK_UNREGISTER =>
            null;  -- TODO

         when EV.PKG_EVENT_CONFLICTS =>
            null;  -- TODO

         when EV.PKG_EVENT_INSTALL_FINISHED |
              EV.PKG_EVENT_EXTRACT_FINISHED |
              EV.PKG_EVENT_DEINSTALL_FINISHED |
              EV.PKG_EVENT_DELETE_FILES_FINISHED |
              EV.PKG_EVENT_UPGRADE_FINISHED |
              EV.PKG_EVENT_FETCHING |
              EV.PKG_EVENT_ARCHIVE_COMP_UNSUP |
              EV.PKG_EVENT_FAILED_CKSUM |
              EV.PKG_EVENT_CREATE_DB_ERROR =>
            null;  -- (really, there's nothing to do)

      end case;


      return True;
   end event_callback;

end Cmd.Event;
