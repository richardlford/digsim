#include "limit.h"
REAL limit( REAL x           // X           - The value to be limited [Real]
          , REAL lower_limit // Lower_Limit - Minimum allowable value for Value [Real]
          , REAL upper_limit // Upper_Limit - Maximum allowable value for Value [Real]
          )
{
  // Outputs:
  //    Limit  -  Bounded value of X [Real]
  if      (x < lower_limit) return lower_limit;
  else if (x > upper_limit) return upper_limit;
  else return x;
}
