#include "drvrinit.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/setval.h"
#include "../driver/print.h"
void digsim_initialization(void) {
  // This subroutine initializes all of the driver variables to before-use
  // conditions; i.e. counters or references are set to 0, etc.
  // Initialize initial values common block variables
  number_of_real_values = 0;
  for (int i = 0; i < MAX_NUMBER_OF_REAL_VALUES; i++) {
    set_real_value[i].r = 0.0;
    set_real_value_index[i] = 0;
  }
  // Initialize print common block variables
  for (int j = 0; j < MAX_NUMBER_OF_PRINTS; j++) {
    print_index[j] = 0;
  }
}
