with global,framework,ada.exceptions;
use  global,framework,ada;
procedure remove_event is
   -- Removes the first event in the future event queue. All subsequent events
   -- are then advanced in the queue.
   event_removal_requested_from_empty_queue : exception;
begin
   if  nevents > 0 then
      for i in 2 .. nevents loop
         event_list(i - 1) := event_list(i);
      end loop;
      event_list(nevents) := (noop,big);
      nevents             := nevents - 1;
  else
     -- Inform user if error condition exists
     raise event_removal_requested_from_empty_queue;
  end if;
end remove_event;
