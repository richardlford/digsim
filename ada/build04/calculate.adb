with global; use global;
procedure calculate is
begin
   -- Calculate derivative at current time
   xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;
end calculate;
