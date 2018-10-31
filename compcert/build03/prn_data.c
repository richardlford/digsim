#include "prn_data.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/print.h"
#include <stdio.h>
void print_data(void) {
  // This subroutine writes print data to standard output.
  // Write run data to standard output
  printf("%.4f",time);
  for (int i = 0; i <= number_of_prints; i++) {
    printf("\t%.6f",real_array[print_index[i]].r);
  }
  printf("\n");
}
