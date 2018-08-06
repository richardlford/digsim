with actuator,aero,mass,kinematics,target,seeker,gyro,fcomp;
use  actuator,aero,mass,kinematics,target,seeker,gyro,fcomp;
procedure calculate is
begin
   --This routine calls the routines that evaluate the state derivatives
   actuator_response;
   aerodynamics_response;
   mass_response;
   kinematics_response;
   target_response;
   seeker_response;
   gyro_response;
   flight_computer_response;
end calculate;
