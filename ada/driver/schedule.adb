with framework,text_io;
use  framework,text_io;
procedure schedule(event : event_type) is
   -- This subroutine schedules the desired events in the queue.
   i : natural;
begin
   -- Check for an empty event queue; if empty, enter event in first slot
  if nevents = 0  then
     nevents := 1;
     event_list(1) := event;
  elsif nevents < event_list'last then
     -- If queue is not full, insert the event such that the events are in chronological order.
     i := nevents;
     nevents := nevents + 1;
     while i >= 1 and then (event.time < event_list(i).time) loop
        event_list(i + 1) := event_list(i);
        i := i - 1;
     end loop;
     event_list(i + 1) := event;
  end if;
exception
   when constraint_error =>
      put_line("############################################################");
      put_line("################## number of events exceeded ###############");
      put_line("############################################################");
      raise;
end schedule;
