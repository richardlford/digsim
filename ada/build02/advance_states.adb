with global; use global;
with deriv; use deriv;
with iomap;
procedure advance_states is
begin
  for i in 1 .. ndes loop
     iomap.cell(x_real(i)).r.all := iomap.cell(x_real(i)).r.all + iomap.cell(xdot_real(i)).r.all*dt;
  end loop;
  -- Advance time to match states
  time := time + dt;
end advance_states;
