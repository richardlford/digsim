with global,ada.numerics.elementary_functions;
use  global,ada.numerics.elementary_functions;
procedure calculate is
begin
   xd           :=  velocity*cos(theta);         -- Calculate velocities
   zd           := -velocity*sin(theta);
   q_s          := (z*xd - x*zd)/(x**2 + z**2);  -- Calculate true LOS rate
   q_s_meas     := q_s;                          -- Calculate measured LOS rate
   thetadot_cmd := guidance_gain*q_s_meas;       -- Calculate guidance comand
   thetadot     := thetadot_cmd;                 -- Calculate airframe rate
end calculate;
