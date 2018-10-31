#include <math.h>
#include "../driver/digtypes.h"

REAL arctan(REAL y, REAL x) { // Output angle [rad]
  // This function computes the arctangent of Y/X. 0 is returned for
  // an input of 0/0.
  // X - In a right triangle, the ratio of the side adjacent to the angle to the
  //     side opposite the angle X [Real]
  return ((x == 0.0) && (y == 0.0)) ? 0.0 : atan2(y,x);
}
