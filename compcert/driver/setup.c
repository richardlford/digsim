#include "../driver/setup.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/setval.h"
#include "../driver/print.h"
#include "../driver/event.h"
#include "../driver/schedule.h"
void setup_driver_for_run() {
  // This subroutine sets up the driver and global common for the next run..
  //
  // Inputs:
  //   MAX_NUMBER_OF_EVENTS  - Maximum allowable number of events in the event queue [Integer]
  //   Number_Of_Prints      - Number of communications array locations to print [Integer]
  //   Number_Of_Real_Values - Current number of real variables to be initialized [Integer]
  //   Set_Real_Value        - Initial values for real variables [Real Array]
  //   Set_Real_Value_Index  - Real_Array locations for initial values [Integer Array]
  //
  // Outputs:
  //   LOG_PRINT_DATA        - Event corresponding to printing data to standard output file [Integer]
  //   Number_Of_Events      - Current number of events in the event queue [Integer]
  //   Events                - Event queue [Integer Array]
  //   Stop_Simulation       - Flag indicating simulation termination [sec]
  //   TERMINATE_SIMULATION  - Event corresponding to simulation termination [Integer]
  //   Time_Of_Events        - Time to execute corresponding event in event queue [Real Array]
  //   TStop                 - Simulation termination time [sec]
  //   Real_Array            - The communications array for real variables [Real Array]

  // Event designations

  // Exceptions to default type

  // Flush the event queue
  INT8 i;
  number_of_events = 0;
  for (i = 0; i < MAX_NUMBER_OF_EVENTS; i++) {
    events[i] = 0;
    time_of_events[i] = 1.0e10;
  }
  // Initialize global common
  for (i = 0; i < number_of_real_values; i++)
    real_array[set_real_value_index[i]] = set_real_value[i];

  // Schedule initial print
  if (number_of_prints > 0)
    schedule(0.0,LOG_PRINT_DATA);
  // Initialize termination flag
  stop_simulation = FALSE;
  schedule(tstop,TERMINATE_SIMULATION);
}
