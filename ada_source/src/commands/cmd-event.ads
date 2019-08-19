--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Event;

package Cmd.Event is

   package EV renames Core.Event;

   --  Set global "quiet" variable for events
   procedure handle_event_callbacks (comline : Cldata);

private

   event_debug    : ST_Debug_Level := ST_Debug_Level'First;
   event_mute     : Boolean := False;
   progress_debit : Boolean := False;
   add_deps_depth : Natural := 0;
   message_buffer : Text;

   --  Main callback for events
   function event_callback (event : EV.pkg_event; data : Text) return Boolean;

   --  Helpers
   --  warn appends ": <strerror>" to message.
   --  warnx just prints the message to stdout verbatim
   procedure warn  (message : String; error : Integer);
   procedure warnx (verbatim_message : String);


end Cmd.Event;
