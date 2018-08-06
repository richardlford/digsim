with schedule;
with global,ada.numerics.elementary_functions,framework;
use  global,ada.numerics.elementary_functions;
procedure calculate is
   dtimpact : float; -- Time until impact [Sec]
begin
   -- Calculate acceleration at current time
   xdd := -gravity;
   -- Check to see if impact will occur within a maximum time step
   if (x + (xd*dtmax) + (0.5*xdd*dtmax*dtmax)) <= 0.0 then
      -- If so, check to see if it is too late
      if (x + (xd*dtmin) + (0.5*xdd*dtmin*dtmin)) <= 0.0 then
         -- If too late, set delay until impact to zero
         dtimpact := 0.0;
      else -- Otherwise, calculate delay until actual impact
         dtimpact := (-xd - sqrt(xd*xd - 2.0*x*xdd))/(2.0*x);
         if dtimpact < dtmin then
            dtimpact := (-xd + sqrt(xd*xd - 2.0*x*xdd))/(2.0*x);
         end if;
      end if;
      -- Schedule impact
      schedule((framework.bounce, time + dtimpact));
   end if;
end calculate;
