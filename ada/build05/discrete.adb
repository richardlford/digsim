with global,framework;
use  global,framework;
procedure discrete is
begin
   -- This subroutine processes the scheduled user-defined discrete events.
   if event_list(1).name = bounce then
      xd := -coef_of_restitution*xd;
  end if;
  recalc := true;
end discrete;
