--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Ada.Calendar;

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

   procedure emit_progress_tick  (prog_current : int64;
                                  prog_total   : int64);

private

   package CAL renames Ada.Calendar;

   --  warn appends ": <strerror>" to message.
   --  warnx just prints the message to stdout verbatim
   procedure warn  (message : String; error : Integer);
   procedure warnx (verbatim_message : String);

   type Progress_Info is
      record
         last_progress_percent : Integer;
         progress_started      : Boolean;
         progress_interrupted  : Boolean;
         progress_debit        : Boolean;
         progress_message      : Text;
         message_buffer        : Text;
         last_tick             : int64;
         stalled               : int64;
         bytes_per_second      : int64;
         last_update           : CAL.Time;
         timer_begin           : CAL.Time;
         have_terminal         : Boolean;
      end record;

   muted        : Boolean := False;
   our_progress : Progress_Info;

   procedure progressbar_stop;
   procedure check_progress;
   procedure draw_progressbar (current, total : int64);

end Core.Event;
