with global,framework,digsimio;
use  global,framework;
procedure advance_states is
   cell : digsimio.cell_list_type renames digsimio.cell;
begin
  for i in 1 .. nstates loop
     cell(xstate(i)).r.all := cell(xstate(i)).r.all + cell(xdotstate(i)).r.all*dt;
  end loop;
  -- Advance time to match states
  time := time + dt;
end advance_states;
