with global,text_io,ada.float_text_io,ada.numerics.elementary_functions;
use  global,text_io,ada.float_text_io,ada.numerics.elementary_functions;
function termination return boolean is
   -- This module determines if the run termination conditions have been met
   procedure calculate_miss is
      -- Evaluate closest approach time and miss distance
      dtmiss  : float;
      rmiss   : float;
      missdat : file_type;
   begin
      create(missdat,out_file,"miss.dat");
      dtmiss := (x*xd + z*zd)/(xd**2 + zd**2);
      rmiss  := sqrt((x - xd*dtmiss)**2 + (z - zd*dtmiss)**2);
      put(missdat,"RMiss   = "); put(missdat,rmiss);  new_line(missdat);
      put(missdat,"Ddtmiss = "); put(missdat,dtmiss); new_line(missdat);
      close(missdat);
   end calculate_miss;
begin
   if z >= 0.0 then
      calculate_miss;
      return true;
   else
      return false;
   end if;
end termination;
