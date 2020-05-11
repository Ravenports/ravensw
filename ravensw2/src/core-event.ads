--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Core.Event is
   procedure mute_event;

   procedure emit_errno          (err_function : String;
                                  err_argument : String;
                                  err_number   : Integer);

   procedure emit_error          (message : String);
   procedure emit_notice         (message : String);
   procedure emit_message        (message : String);
   procedure emit_developer_mode (message : String);
   procedure emit_progress_start (message : String);
   procedure emit_with_strerror  (message : String);
   procedure emit_no_local_db;

   procedure emit_debug          (debug_level : ST_Debug_Level;
                                  debug_msg   : String);

--     procedure event_progress_tick  (prog_current : T_progress_tick;
--                                     prog_total   : T_progress_tick);

private

   muted         : Boolean := False;

   --  warn appends ": <strerror>" to message.
   --  warnx just prints the message to stdout verbatim
   procedure warn  (message : String; error : Integer);
   procedure warnx (verbatim_message : String);

end Core.Event;
