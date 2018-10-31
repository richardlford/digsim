#include "term.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
BOOL termination_conditions(void) {
  // This module determines if the run termination conditions have been met
  return time >= tstop ? TRUE : FALSE;  // Stop simulation run [Boolean]
}
