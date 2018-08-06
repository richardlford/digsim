with ada.numerics.elementary_functions;
use  ada.numerics.elementary_functions;
function atan2(y,x : float) return float is
  -- Output angle [rad].
  -- This function computes the arctangent of Y/X. 0 is returned for an input of 0/0.
  -- X - In a right triangle, the ratio of the side adjacent to the angle to the
  --  side opposite the angle X [Real]
begin
  if x = 0.0 and y = 0.0 then
  return 0.0;
  else
     return arctan(y,x);
  end if;
end atan2;
