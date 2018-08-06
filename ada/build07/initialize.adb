with kinematics,seeker,fcomp,airframe;
use  kinematics,seeker,fcomp,airframe;
with define_state;
with schedule;
with events,framework,digsimio,global;
use  events,framework;
procedure initialize is
begin
   -- load user configuration
   declare
      use digsimio;
   begin
      for i in 1 .. nsets loop
         case set_list(i).kind is
            when real => cell(set_list(i).name).r.all := set_list(i).r;
            when int8 => cell(set_list(i).name).i.all := set_list(i).i;
            when bool => cell(set_list(i).name).b.all := set_list(i).b;
         end case;
      end loop;
   end;

   -- initialize globals
   declare
      use global;
   begin
      time := time0;  -- Set initial time
      quit := false;  -- Initialize termination flag
      schedule((terminate_simulation,tstop));
   end;

   -- Schedule initial print
   if nprints > 0 then
      schedule((log_print_data,0.0));
   end if;

   kinematics_init;
   seeker_init;
   flight_computer_init;
   airframe_response_init;
end initialize;
