#include <stdio.h>
#include <stdlib.h>
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/parser.h"
#include "../driver/setval.h"

void set_card() {
  // This subroutine decodes a "SET" card; which initializes the specified global
  // common location to the specified value. The format is:
  //
  //     SET (Variable) = (Value) (Location)
  int var_loc;
  // If there's space left...
  if (number_of_real_values < MAX_NUMBER_OF_REAL_VALUES) {
    // Get location
    var_loc = atoi(tokens[2]);
    // Store global common location & initial value
    number_of_real_values = number_of_real_values + 1;
    set_real_value_index[number_of_real_values] = var_loc;
    set_real_value[number_of_real_values].r = atof(tokens[1]);
  }
  else {
    printf(" Too many SET commands\n");
    printf(" SET command not executed\n");
  }
}
