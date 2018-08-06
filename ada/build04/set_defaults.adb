with global;
use  global;
procedure set_defaults is
begin
   -- Set model independent data and reset time
   dt       := 0.01;
   time0    := 0.0;
   tstop    := 2.5;
   dtmax    := 0.01;
   dtmin    := 0.001;
   dtprint  := 0.05;
   --  Set the default data for the simulation
   damping_coefficient := 8.88;
   gravity             := 9.88;
   mass                := 1.0;
   spring_coefficient  := 39.47;
   x_ic                := 0.0;
   xd_ic               := 0.0;
end set_defaults;
