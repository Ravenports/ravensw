--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Arithmetic;
with Ada.Characters.Latin_1;
with Ada.Environment_Variables;
with Ada.Text_IO;

with Core.Strings; use Core.Strings;
with Core.Context;
with Core.Unix;
with Core.Utilities;


package body Core.Event is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;
   package ENV renames Ada.Environment_Variables;

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
                         err_number   : Integer) is
   begin
      check_progress;
      warn (err_function & '(' & err_argument & ')', err_number);
   end emit_errno;


   --------------------------------------------------------------------
   --  emit_error
   --------------------------------------------------------------------
   procedure emit_error (message : String) is
   begin
      check_progress;
      warnx (message);
   end emit_error;


   --------------------------------------------------------------------
   --  emit_with_strerror
   --------------------------------------------------------------------
   procedure emit_with_strerror (message : String)
   is
      err_number : constant Integer := Unix.errno;
   begin
      check_progress;
      warn (message, err_number);
   end emit_with_strerror;


   --------------------------------------------------------------------
   --  emit_notice
   --------------------------------------------------------------------
   procedure emit_notice (message : String) is
   begin
      check_progress;
      if not muted then
         TIO.Put_Line (message);
      end if;
   end emit_notice;


   --------------------------------------------------------------------
   --  emit_message
   --------------------------------------------------------------------
   procedure emit_message (message : String) is
   begin
      check_progress;
      TIO.Put_Line (message);
   end emit_message;


   --------------------------------------------------------------------
   --  emit_developer_mode
   --------------------------------------------------------------------
   procedure emit_developer_mode (message : String) is
   begin
      check_progress;
      warnx ("DEVELOPER_MODE: " & message);
   end emit_developer_mode;


   --------------------------------------------------------------------
   --  emit_debug
   --------------------------------------------------------------------
   procedure emit_debug (debug_level : ST_Debug_Level;
                         debug_msg   : String) is
   begin
      check_progress;
      if debug_level <= Context.reveal_debug_level then
         warnx ("DBG(" & int2str (debug_level) & ")[" &
                  int2str (Integer (Unix.getpid)) & "]> " & debug_msg);
      end if;
   end emit_debug;


   --------------------------------------------------------------------
   --  emit_no_local_db
   --------------------------------------------------------------------
   procedure emit_no_local_db is
   begin
      check_progress;
      warnx ("Local package database nonexistent!");
   end emit_no_local_db;

   --------------------------------------------------------------------
   --  emit_progress_start
   --------------------------------------------------------------------
   procedure emit_progress_start (message : String) is
   begin
      check_progress;
      if muted then
         return;
      end if;

      our_progress.progress_started      := True;
      our_progress.progress_interrupted  := False;
      our_progress.progress_debit        := False;
      our_progress.last_progress_percent := -1;
      our_progress.last_tick             := 0;
      our_progress.stalled               := 0;
      our_progress.bytes_per_second      := 0;
      our_progress.last_update           := CAL.Clock;
      our_progress.have_terminal         := Unix.screen_attached;

      if IsBlank (message) then
         our_progress.progress_message := our_progress.message_buffer;
      else
         our_progress.progress_message := SUS (message);
      end if;

      if our_progress.have_terminal then
         TIO.Put (USS (our_progress.progress_message) & ":   0%");
      else
         TIO.Put (USS (our_progress.progress_message) & ": ");
      end if;
   end emit_progress_start;


   --------------------------------------------------------------------
   --  progressbar_stop
   --------------------------------------------------------------------
   procedure progressbar_stop is
   begin
      if our_progress.progress_started then
         if Unix.screen_attached then
            TIO.Put_Line ("");
         else
            TIO.Put_Line ("done");
         end if;
      end if;
      our_progress.last_progress_percent := -1;
      our_progress.progress_started      := False;
      our_progress.progress_interrupted  := False;
   end progressbar_stop;


   --------------------------------------------------------------------
   --  check_progress
   --------------------------------------------------------------------
   procedure check_progress is
   begin
      --
      --  If a progressbar has been interrupted by another event, then
      --  we need to add a newline to prevent bad formatting.
      --
      if our_progress.progress_started and then
        not our_progress.progress_interrupted
      then
         TIO.Put_Line ("");
         our_progress.progress_interrupted := True;
      end if;
   end check_progress;


   --------------------------------------------------------------------
   --  draw_progressbar
   --------------------------------------------------------------------
   procedure draw_progressbar (current, total : int64)
   is
      use type CAL.Arithmetic.Day_Count;
      subtype Percentage_Point is Natural range 0 .. 100;

      now     : CAL.Time;
      elapsed : int64;
      days    : CAL.Arithmetic.Day_Count;
      seconds : Duration;
      leapsec : CAL.Arithmetic.Leap_Seconds_Count;
      percent : Percentage_Point;
   begin
      if not our_progress.progress_started then
         progressbar_stop;
         return;
      end if;

      if current < 0 then
         TIO.Put_Line (TIO.Standard_Error, "Progress bar feed a negative current value");
         progressbar_stop;
         return;
      end if;

      if total = 0 then
         TIO.Put_Line (TIO.Standard_Error, "Progress bar given zero total items");
         progressbar_stop;
         return;
      end if;

      if our_progress.progress_debit then
         now := CAL.Clock;
         CAL.Arithmetic.Difference (Left         => now,
                                    Right        => our_progress.last_update,
                                    Days         => days,
                                    Seconds      => seconds,
                                    Leap_Seconds => leapsec);
         elapsed := int64 (days) * 86400 + int64 (seconds) + int64 (leapsec);
         if elapsed < 0 then
            elapsed := 0;
         end if;
      end if;

      percent := Percentage_Point (current * 100 / total);
      --
      --  Wait for interval for debit bars to keep calc per second.
      --  If not debit, show on every % change, or if ticking after
      --  an interruption (which removed our progressbar output).
      --
      if current >= total or else
        (our_progress.progress_debit and then elapsed >= 1) or else
        (not our_progress.progress_debit and then
           (Integer (percent) /= our_progress.last_progress_percent or else
            our_progress.progress_interrupted))
      then
         our_progress.last_progress_percent := Integer (percent);
         TIO.Put (LAT.CR & USS (our_progress.progress_message) & ": "
                  & pad_right (int2str (Integer (percent)) & LAT.Percent_Sign, 4));
         if our_progress.progress_debit then
            declare
               procedure insert_fragment (S : String);
               transferred : int64;
               bytes_left  : int64;
               cur_speed   : int64;
               since_begin : int64;
               age_factor  : Float;
               line        : String (1 .. 40);
               line_end    : Natural := 0;

               procedure insert_fragment (S : String)
               is
                  L_start : Positive := line_end + 1;
               begin
                  line_end := line_end + S'Length;
                  line (L_start .. line_end) := S;
               end insert_fragment;
            begin
               transferred := current - our_progress.last_tick;
               our_progress.last_tick := current;
               bytes_left := total - current;

               CAL.Arithmetic.Difference (Left         => now,
                                          Right        => our_progress.timer_begin,
                                          Days         => days,
                                          Seconds      => seconds,
                                          Leap_Seconds => leapsec);
               since_begin := int64 (days) * 86400 + int64 (seconds) + int64 (leapsec);

               if bytes_left <= 0 then
                  --  We're done, Show final statistics instead of progress
                  --  Indicate event took at least one second.
                  if since_begin <= 0 then
                     elapsed := 1;
                  else
                     elapsed := since_begin;
                  end if;
                  --  Calculate true total speed when done
                  transferred := total;
                  our_progress.bytes_per_second := 0;
               end if;

               if elapsed = 0 then
                  --  occurs when now() is same as our_progress.last_update but transfer
                  --  hasn't been completed
                  cur_speed := transferred;
               else
                  cur_speed := transferred / elapsed;
               end if;

               if since_begin <= 3 then
                  age_factor := 0.4;
               else
                  age_factor := 0.9;
               end if;

               if our_progress.bytes_per_second = 0 then
                  our_progress.bytes_per_second := cur_speed;
               else
                  our_progress.bytes_per_second :=
                    int64 ((Float (our_progress.bytes_per_second) * age_factor) +
                           (Float (cur_speed) * (1.0 - age_factor)));
               end if;

               insert_fragment (" " & Utilities.format_bytes_IEC (current));
               if bytes_left > 0 then
                  insert_fragment (" " & Utilities.format_bytes_SI (transferred) & "/s ");
               else
                  insert_fragment
                    (" " & Utilities.format_bytes_SI (our_progress.bytes_per_second) & "/s ");
               end if;

               if transferred = 0 then
                  our_progress.stalled := 0;
               else
                  our_progress.stalled := elapsed;
               end if;

               if our_progress.stalled >= 5 then
                  insert_fragment (" - stalled -");
               elsif our_progress.bytes_per_second = 0 and then bytes_left > 0 then
                  insert_fragment ("   --:-- ETA");
               else
                  declare
                     disp_seconds : Integer;
                     disp_hours   : Integer;
                     disp_minutes : Integer;
                  begin
                     if bytes_left > 0 then
                        disp_seconds := Integer (bytes_left / our_progress.bytes_per_second);
                     else
                        disp_seconds := Integer (elapsed);
                     end if;
                     disp_hours := disp_seconds / 3600;
                     disp_seconds := disp_seconds - (disp_hours * 3600);
                     disp_minutes := disp_seconds / 60;
                     disp_seconds := disp_seconds - (disp_minutes * 60);

                     if disp_hours = 0 then
                        insert_fragment ("   "
                                         & zeropad (disp_minutes, 2) & ":"
                                         & zeropad (disp_seconds, 2));
                     else
                        insert_fragment (zeropad (disp_minutes, 2) & ":"
                                         & zeropad (disp_minutes, 2) & ":"
                                         & zeropad (disp_seconds, 2));
                     end if;
                  end;
                  if bytes_left > 0 then
                     insert_fragment (" ETA");
                  end if;
               end if;
               TIO.Put (line (line'First .. line_end));
               our_progress.last_update := now;
            end;
         end if;
         TIO.Flush;
      end if;

      if current >= total then
         progressbar_stop;
      end if;

   end draw_progressbar;


   --------------------------------------------------------------------
   --  emit_progress_tick
   --------------------------------------------------------------------
   procedure emit_progress_tick  (prog_current : int64;
                                  prog_total   : int64)
   is
      subtype Percentage_Point is Natural range 0 .. 100;
      percent : Percentage_Point;
      NO_TICK : constant String := "NO_TICK";
   begin
      if not muted and then our_progress.progress_started then
         if Unix.screen_attached then
            draw_progressbar (prog_current, prog_total);
         else
            if our_progress.progress_interrupted then
               TIO.Put (USS (our_progress.progress_message) & "...");
            else
               if not ENV.Exists (NO_TICK) then
                  if prog_total = 0 then
                     percent := 100;
                  else
                     percent := Percentage_Point (prog_current * 100 / prog_total);
                  end if;
                  if our_progress.last_progress_percent / 10 < percent / 10 then
                     our_progress.last_progress_percent := percent;
                     TIO.Put (".");
                     TIO.Flush;
                  end if;
               end if;
               if prog_current >= prog_total then
                  progressbar_stop;
               end if;
            end if;
         end if;
      end if;
      our_progress.progress_interrupted := False;
   end emit_progress_tick;

end Core.Event;
