with framework,global,digsimio;
use  framework,global,digsimio;
procedure reset is
begin
   nplots        := 0;
   nprints       := 0;
   nsets         := 0;
   nstates       := 0;
   global.recalc := false;
   global.quit   := false;
   global.over   := false;
   -- flush event queue
   nevents := 0;
   for i in event_list'range loop
      event_list(i) := (noop, big);
   end loop;

   for name in names loop
      case cell(name).kind is
         when real => cell(name).r.all := 0.0;
         when int8 => cell(name).i.all := 0;
         when bool => cell(name).b.all := false;
      end case;
   end loop;
end reset;

