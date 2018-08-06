with define_state;
with schedule;
with framework,digsimio,global;
use  framework;
procedure initialize is
begin
   -- load user configuration
   declare
      use digsimio;
   begin
      for i in 1 .. nsets loop
         case set_list(i).kind is
            when real => cell(set_list(i).name).r.all := set_list(i).r;
            when int8 => cell(set_list(i).name).i.all := set_list(i).i;
            when bool => cell(set_list(i).name).b.all := set_list(i).b;
         end case;
      end loop;
      --  Setup integrators
      define_state(x,xd);
      define_state(z,zd);
      define_state(theta,thetadot);
   end;

   -- initialize globals
   declare
      use global;
   begin
      -- Set initial time
      time := time0;
      -- Initialize states
      x     := x_ic;
      z     := z_ic;
      theta := theta_ic_dg/rdtodg;
      -- Initialize termination flag
      quit := false;
      schedule((terminate_simulation,tstop));
   end;

   -- Schedule initial print
   if nprints > 0 then
      schedule((log_print_data,0.0));
   end if;
end initialize;
