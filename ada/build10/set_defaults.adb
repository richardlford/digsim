with  global,actuator,aero,mass,kinematics,gyro,target,seeker,fcomp;
use  global,actuator,aero,mass,kinematics,gyro,target,seeker,fcomp;
procedure set_Defaults is
   -- This subroutine calls the routines responsible for setting default data
   -- values. It also sets default system values for this simulation.
begin
   -- Set model independent data and reset time
   Dt      :=  0.005;
   DtMax   :=  0.005;
   DtMin   :=  0.001;
   DtPrint :=  0.01;
   Time0   :=  0.0;
   Tstop   := 10.0;
   -- routines to set default data
   actuator_data;
   aerodynamics_data;
   mass_data;
   kinematics_data;
   gyro_data;
   target_data;
   seeker_data;
   flight_computer_data;
end set_defaults;
