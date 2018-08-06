with airframe,kinematics,gyro,target,seeker,fcomp,global;
use  airframe,kinematics,gyro,target,seeker,fcomp,global;
procedure set_defaults is
   -- This subroutine calls the routines responsible for setting default data
   -- values.  It also sets default system values for this simulation.
begin
   -- Set model independent data and reset time;
   dt      :=  0.005;
   dtmax   :=  0.005;
   dtmin   :=  0.001;
   dtprint :=  0.01;
   time    :=  0.0;
   tstop   := 10.0;
   -- Call routines to set default data;
   airframe_data;
   kinematics_data;
   gyro_data;
   target_data;
   seeker_data;
   flight_computer_data;
end set_defaults;
