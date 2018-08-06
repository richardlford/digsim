with kinematics,seeker,fcomp,airframe,global;
use  kinematics,seeker,fcomp,airframe,global;
procedure set_defaults is
begin
  -- Set model independent data and reset time
  dtmax   :=   0.005;
  dtmin   :=   0.001;
  dtprint :=   0.01;
  time0   :=   0.0;
  tstop   :=  10.0;
  kinematics_data;
  seeker_data;
  flight_computer_data;
  airframe_response_data;
end set_defaults;
