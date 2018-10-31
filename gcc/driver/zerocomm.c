#include "../driver/global.h"
void zero_global_common() {
  // Zero global common
  for (int i = 1; i < REAL_ARRAY_SIZE; i++)
    real_array[i].i = 0;
}
