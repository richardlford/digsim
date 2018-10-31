#include <math.h>
#include "../driver/prn_data.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/print.h"
#include "../driver/event.h"
#include "../driver/schedule.h"
void print_data() {
  // This subroutine writes print data to standard output.
  //
  // Inputs:
  //    DtPrint          - Time step between printing data [sec]
  //    Number_Of_Prints - Number of communications array locations to print [Integer]
  //    Print_Index      - Communications array location of print data [Integer Array]
  //    Real_Array       - The communications array for real variables [Real Array]
  //    Time             - Simulation time [sec]
  //
  // Outputs:
  //    LOG_PRINT_DATA   - Event corresponding to printing data to standard output file [Integer]
  // Write run data to standard output
  printf("%.4f",time);
  for (int i = 0; i < number_of_prints; i++)
    printf("\t%.6f", real_array[print_index[i]].r);
  printf("\n");
  // Schedule next print
  // CALL SCHEDULE(((NINT(time/dtprint)+1)*dtprint),log_print_data)
  schedule((roundf(time/dtprint) + 1.0)*dtprint, LOG_PRINT_DATA);
}
