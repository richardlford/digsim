#include "../driver/advance.h"
#include "../driver/deriv.h"
#include "../driver/digtypes.h"
#include "../driver/drvrdata.h"
#include "../driver/event.h"
#include "../driver/global.h"
#include "../driver/input.h"
#include "../driver/setup.h"
#include "../driver/sysvars.h"
#include "../driver/zerocomm.h"
#include "../driver/procevnt.h"
#include "def_data.h"
#include "diffeq.h"
#include "drvrinit.h"
#include "init.h"
#include "prn_data.h"
#include "term.h"
int main() {
  // DigSim provides a FORTRAN architecture required to simulate
  // continuous systems described by sets of simultaneous first-order
  // differential equations.
  //
  // Variables:
  //   End_Of_Run      - Flag set by user to stop the simulation  [Logical]
  //   Ndes_Real       - Number of real states [Integer]
  //   ReEval_Derivs   - Indicates states changed value at a discrete event [Logical]
  //   Stop_Simulation - Flag indicating simulation termination [sec]
  BOOL end_of_run  = FALSE;
  BOOL execute_run = FALSE;
  //  Open the input file
  FILE *input;
  input = fopen("input.dat","r");
  // Global driver initialization
  digsim_initialization();
  driver_default_data();
  execute_run = input_data(input);   // Get data for first run
  // If data is available, begin loop to execute simulation runs.
  while (TRUE == execute_run) {
    // Setup the driver for this run
    end_of_run = FALSE;
    zero_global_common();
    driver_default_data();
    default_data();
    setup_driver_for_run();
    ndes_real = -1;
    initialization();
    // States at current time are known; calculate state derivatives at current
    // time.
    differential_equations();
    // Process the initial discrete events
    process_events();
    // Recalculate derivat:ves if so directed by user
    if (TRUE == reeval_derivs) {
      differential_equations();
      reeval_derivs = FALSE;
    }
    // Advance the simulation states until termination.
    while (end_of_run != TRUE && stop_simulation != TRUE) {
      // Advance states to new time
      advance_states();
      // Calculate state derivatives at new time
      differential_equations();
      // Process discrete events based on current information
      process_events();
      // Recalculate derivatives if so directed by user
      if (TRUE == reeval_derivs) {
        differential_equations();
        reeval_derivs = FALSE;
      }
      // Check the termination conditions
      end_of_run = termination_conditions(end_of_run);
    }
    // Setup for next run
    execute_run = input_data(input);
    // End of run execution loop
  }
  fclose(input);
}
