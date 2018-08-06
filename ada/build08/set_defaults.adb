with global,airframe,target,seeker,fcomp,kinematics;
use  global,airframe,target,seeker,fcomp,kinematics;
procedure set_defaults is
begin
  -- Set model independent data and reset time
  dtmax   := 0.005;
  dtmin   := 0.001;
  dtprint := 0.01;
  time0   := 0.0;
  tstop   := 10.0;
  airframe_response_data;
  target_data;
  seeker_data;
  flight_computer_data;
  kinematics_data;
end set_defaults;
