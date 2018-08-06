-- This program simulates a mass-spring-damper system.
with ada.float_text_io; use ada.float_text_io;
with ada.text_io; use ada.text_io;
procedure digsim is
   damping_coefficient : float :=  8.88;  -- Damping force per velocity [N/m/s]
   dt                  : float :=  0.01;  -- Simulation time step [sec]
   gravity             : float :=  9.88;  -- Acceleration due to gravity [m/sec**2)
   mass                : float :=  1.0;   -- Mass suspended from spring [Kg]
   spring_coefficient  : float := 39.47;  -- Restoring force per position [N/m]
   time                : float :=  0.0;   -- Simulation time [sec]
   tstop               : float :=  2.5;   -- Simulation stop time [sec]
   x                   : float :=  0.0;   -- Position of suspended mass [m]
   x_ic                : float :=  0.0;   -- Initial position of suspended mass [m]
   xd                  : float :=  0.0;   -- Velocity of suspended mass [m/sec]
   xd_ic               : float :=  0.0;   -- Initial velocity of suspended mass [m/sec]
   xdd                 : float :=  0.0;   -- Acceleration of suspended mass [m/sec**2]
begin
  -- Set initial conditions
  x    := x_ic;
  xd   := xd_ic;
  time := 0.0;
  -- Main simulation loop
  while time <= tstop loop
     -- Print states for this time
     Put(time); put(" "); put(x); put(" "); put(xd); new_line;
     -- Calculate derivative at current time
     xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;
     -- Advance states one time step
     x  := x + xd*dt;
     xd := xd + xdd*dt;
     -- Advance time and continue simulation loop
     time := time + dt;
  end loop;
end digsim;
