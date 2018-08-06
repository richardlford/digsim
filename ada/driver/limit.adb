function limit (x,lower_limit,upper_limit : float) return float is
  --  This function bounds X between Lower_Limit and Upper_Limit.
  --  Lower_Limit - Minimum allowable value for Value [Real]
  --  Upper_Limit - Maximum allowable value for Value [Real]
  --  X           - The value to be limited [Real]
  --  Output: Bounded value of X [Real]
begin
  if x < lower_limit then
     return lower_limit;
  elsif x > upper_limit then
     return upper_limit;
  else
     return x;
  end if;
end limit;
