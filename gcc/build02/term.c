#include "global.h"
#include "sysvars.h"
BOOL termination_conditions() {
  // Stop simulation run [Boolean]
  return time >= tstop ? TRUE : FALSE;
}
