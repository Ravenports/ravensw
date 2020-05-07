--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;

with Core.Strings; use Core.Strings;
with Core.Context;
with Core.Unix;


package body Core.Event is

   package TIO renames Ada.Text_IO;


   --------------------------------------------------------------------
   --  mute_event
   --------------------------------------------------------------------
   procedure mute_event is
   begin
      muted := True;                --  TODO: Should this be in context?
   end mute_event;


   --------------------------------------------------------------------
   --  warn
   --------------------------------------------------------------------
   procedure warn  (message : String; error : Integer)
   is
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
   --  emit_errno
   --------------------------------------------------------------------
   procedure emit_errno (err_function : String;
                         err_argument : String;
                         err_number   : Integer)
   is
   begin
      warn (err_function & '(' & err_argument & ')', err_number);
   end emit_errno;


   --------------------------------------------------------------------
   --  emit_error
   --------------------------------------------------------------------
   procedure emit_error (message : String) is
   begin
      warnx (message);
   end emit_error;


   --------------------------------------------------------------------
   --  emit_with_strerror
   --------------------------------------------------------------------
   procedure emit_with_strerror (message : String)
   is
      err_number : constant Integer := Unix.errno;
   begin
      warn (message, err_number);
   end emit_with_strerror;


   --------------------------------------------------------------------
   --  emit_notice
   --------------------------------------------------------------------
   procedure emit_notice (message : String) is
   begin
      if not muted then
         TIO.Put_Line (message);
      end if;
   end emit_notice;


   --------------------------------------------------------------------
   --  emit_message
   --------------------------------------------------------------------
   procedure emit_message (message : String) is
   begin
      TIO.Put_Line (message);
   end emit_message;


   --------------------------------------------------------------------
   --  emit_developer_mode
   --------------------------------------------------------------------
   procedure emit_developer_mode (message : String) is
   begin
      warnx ("DEVELOPER_MODE: " & message);
   end emit_developer_mode;


   --------------------------------------------------------------------
   --  emit_progress_start
   --------------------------------------------------------------------
   procedure emit_progress_start (message : String) is
   begin
      --  TODO: progressbar_start(ev->e_progress_start.msg);
      null;
   end emit_progress_start;


   --------------------------------------------------------------------
   --  emit_debug
   --------------------------------------------------------------------
   procedure emit_debug (debug_level : ST_Debug_Level;
                         debug_msg   : String)
   is
   begin
      if debug_level >= Context.reveal_debug_level then
         warnx ("DBG(" & int2str (debug_level) & ")[" &
                  int2str (Integer (Unix.getpid)) & "]> " & debug_msg);
      end if;
   end emit_debug;


   --------------------------------------------------------------------
   --  emit_progress_tick
   --------------------------------------------------------------------
--     procedure emit_progress_tick  (prog_current : T_progress_tick;
--                                    prog_total   : T_progress_tick)
--     is
--     begin
--        --  TODO: progressbar_tick(ev->e_progress_tick.current,
--        --  ev->e_progress_tick.total);
--        null;
--     end emit_progress_tick;

end Core.Event;
