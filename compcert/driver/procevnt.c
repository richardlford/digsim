#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/event.h"
#include "../driver/prn_data.h"
#include "../driver/rmevent.h"
#include "discrete.h"

void process_events() {
  // This subroutine processes the various discrete events. If the
  // event is a system event, the appropriate system routine is called;
  // if it is a user event (i.e. greater than 9) the user discrete
  // processing routine, Descrete, is called. The user is responsible
  // for monitoring for illegal user events.
  //
  // Inputs:
  //    DtMax                - Maximum allowable integration step size [sec]
  //    DtMin                - Minimum allowable integration step size [sec]
  //    Events               - Event queue [Integer Array]
  //    Number_Of_Events     - Current number of events in the event queue [Integer]
  //    Time                 - Simulation time [sec]
  //    Time_Of_Events       - Time to execute corresponding event in event queue [Real Array]
  //
  // Outputs:
  //    Dt                   - Integration step size [sec]
  //    Stop_Simulation      - Flag indicating simulation termination [sec]
  //
  // Internal variables and constants:
  //    LOG_PRINT_DATA       - Event corresponding to printing data to standard output file [Integer]
  //    More_Events          - Flag indicating that the remaining events are scheduled
  //                           for more than DtMax from now (Logical)
  //    TERMINATE_SIMULATION - Event corresponding to simulation termination [Integer]
  //    Time_To_Next_Event   - Unadjusted time step to next discrete event [Real]

  BOOL more_events;
  REAL time_to_next_event;
  // Process events as long as there are events remaining within this time step
  if (number_of_events > 0) {
    while ((time_of_events[0]-time) < dtmin) {
      // Process current event
      // Write out data to standard output
      if (events[0] == LOG_PRINT_DATA)
        print_data();
      // Check for simulation termination
      else if (events[0] == TERMINATE_SIMULATION)
        stop_simulation = TRUE;
      // Check for user event
      else if (events[0] >= 10)
        discrete();
      // If no match to above cases, write error message and go to next event.
      else
        printf(" Undefined system event scheduled\n");
      // Remove event from queue and process next event
      remove_event();
    }
    // Adjust Dt as necessary
    time_to_next_event = time_of_events[0] - time;
    more_events = number_of_events > 0;
    if ((time_to_next_event < dtmax) && more_events)
      dt = time_to_next_event;
    else
      dt = dtmax;
  }
  else
    dt = dtmax;
}
