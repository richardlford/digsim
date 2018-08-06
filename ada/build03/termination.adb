with global;
use  global;
function termination return boolean is
   -- Quit : Stop simulation run [Boolean]
begin
  return time >= tstop;
end termination;
