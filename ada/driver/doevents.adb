with discrete;
with print_data;
with remove_event;
with framework,text_io,global;
use  framework,text_io,global;
procedure doevents(output : file_type) is
   -- This subroutine processes the various discrete events. If the event is a system event, the
   -- appropriate system routine is called; if it is a user event, the user discrete processing
   -- routine Descrete is called. The user is responsible for monitoring for illegal user events.
   more_events        : boolean;
   time_to_next_event : float;
begin
   -- Process events as long as there are events remaining within this time step
   if nevents > 0 then
      while event_list(1).time - time < dtmin loop
         -- Process current event
         case event_list(1).name is
            when log_print_data       => print_data(output);
            when terminate_simulation => quit := true;
            when bounce               => discrete;
            when noop =>
               put_line("############################################################");
               put_line("################## noop event called #######################");
               put_line("############################################################");
         end case;
         remove_event; -- Remove event from queue and process next event
      end loop;
      -- Adjust Dt as necessary
      time_to_next_event := event_list(1).time - time;
      more_events := nevents > 0;
      if (time_to_next_event < dtmax) and more_events then
         dt := time_to_next_event;
      else
         dt := dtmax;
      end if;
   else
      dt := dtmax;
   end if;
end doevents;
