with global;
function termination return boolean is
   -- This module determines if the run termination conditions have been met
begin
   return global.quit;
end termination;
