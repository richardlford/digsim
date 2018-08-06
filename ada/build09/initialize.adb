with airframe,kinematics,gyro,target,seeker,fcomp,global;
use  airframe,kinematics,gyro,target,seeker,fcomp,global;
with define_state;
with schedule;
with framework,digsimio,global;
use  framework;
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

   -- Schedule initial print
   if nprints > 0 then
      schedule((log_print_data,0.0));
   end if;

   -- Set initial time
   time := time0;  -- Set initial time
   quit := false;  -- Initialize termination flag
   schedule((terminate_simulation,tstop));

   -- Initialize models
   airframe_init;
   kinematics_init;
   gyro_init;
   target_init;
   seeker_init;
   flight_computer_init;
end initialize;
