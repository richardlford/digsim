with global; use global;
with text_io, ada.float_text_io;
use  text_io, ada.float_text_io;
procedure differential_equations is
begin
   -- Print states for this time
   put(output,time); put(output," ");
   put(output,x); put(output," ");
   put(output,xd);
   new_line(output);
   -- Calculate derivative at current time
   xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;
end differential_equations;
