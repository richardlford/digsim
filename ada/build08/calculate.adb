with airframe,target,seeker,fcomp,kinematics;
use  airframe,target,seeker,fcomp,kinematics;
procedure calculate is
  -- This routine calls the modules that evaluate the state derivatives for the
  -- terminal homing simulation
begin
  airframe_response;
  target_response;
  seeker_response;
  flight_computer_response;
  kinematics_response;
end calculate;
