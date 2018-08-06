with global;
use  global;
procedure set_defaults is
begin
   -- Set model independent data and reset time
   dt       := 0.01;
   time0    := 0.0;
   tstop    := 10.0;
   Dtmax    := 0.005;
   DtMin    := 0.001;
   dtprint  := 0.01;

   --  Set the default data for the simulation
   coef_of_restitution := 0.8;
   gravity := 9.88;
   x_ic    := 10.0;
   xd_ic   := 0.0;
end set_defaults;
