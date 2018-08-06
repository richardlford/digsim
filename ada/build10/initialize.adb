with schedule;
with framework,actuator,aero,mass,kinematics,gyro,target,seeker,fcomp;
use  framework,actuator,aero,mass,kinematics,gyro,target,seeker,fcomp;
with global,digsimio;
procedure initialize is
   -- This routine initializes state values and state counters and pointers;
   -- then calls the routines to initialize the various models.
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

   -- Initialize models
   actuator_init;
   aerodynamics_init;
   mass_init;
   kinematics_init;
   gyro_init;
   target_init;
   seeker_init;
   flight_computer_init;
end initialize;
