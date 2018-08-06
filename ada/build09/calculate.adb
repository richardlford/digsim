with airframe,kinematics,target,seeker,gyro,fcomp;
use  airframe,kinematics,target,seeker,gyro,fcomp;
procedure calculate is
   -- This routine calls the routines that evaluate the state derivatives
begin
  airframe_response;
  kinematics_response;
  target_response;
  seeker_response;
  gyro_response;
  flight_computer_response;
END calculate;
