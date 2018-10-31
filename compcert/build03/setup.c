#include "setup.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/setval.h"
void setup_driver_for_run(void) {
  // Initialize global common
  for (int i = 0; i < number_of_real_values; i++)
    real_array[set_real_value_index[i]].r = set_real_value[i].r;
}
