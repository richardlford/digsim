with global;
use  global;
procedure set_defaults is
begin
  -- Set model independent data and reset time
  dtmax   :=   0.005;
  dtmin   :=   0.001;
  dtprint :=   0.01;
  time0   :=   0.0;
  tstop   :=  10.0;

   -- Set the default data for the simulation
  theta_ic_dg   :=    0.0;
  velocity      :=  100.0;
  x_ic          := -500.0;
  z_ic          := -100.0;
  guidance_gain :=    3.0;
end set_defaults;
