with global,deriv,iomap,text_io;
use global,deriv,text_io;
procedure initialization is
begin
   -- Set file handles
   create(output, out_file, "output.dat");
   -- Set initial time
   time := time0;
   --  Setup integrators
   ndes := 2;
   x_real   (1) := iomap.x;
   xdot_real(1) := iomap.xd;
   x_real   (2) := iomap.xd;
   xdot_real(2) := iomap.xdd;
   -- Initialize states
   x  := x_ic;
   xd := xd_ic;
end initialization;
