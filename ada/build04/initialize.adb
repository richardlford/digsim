with define_state;
with schedule;
with global,digsimio;
use  digsimio;
procedure initialize is
begin
   -- load user configuration
   for i in 1 .. ncfgs loop
      case cfgval(i).kind is
         when real => cell(cfgval(i).name).r.all := cfgval(i).r;
         when int8 => cell(cfgval(i).name).i.all := cfgval(i).i;
         when bool => cell(cfgval(i).name).b.all := cfgval(i).b;
      end case;
   end loop;
   -- Set initial time
   global.time := global.time0;
   --  Setup integrators
   define_state(x ,xd );
   define_state(xd,xdd);
   -- Initialize states
   global.x  := global.x_ic;
   global.xd := global.xd_ic;
   -- Schedule initial print
   if nprints > 0 then
      schedule(0.0,log_print_data);
   end if;
  -- Initialize termination flag
   global.quit := false;
   schedule(global.tstop,terminate_simulation);
end initialize;
