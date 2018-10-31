#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/deriv.h"
#include "../driver/drvrdata.h"
#include "../driver/input.h"
#include "../driver/zerocomm.h"
#include "../driver/advance.h"
#include "../driver/digtypes.h"
#include "def_data.h"
#include "diffeq.h"
#include "drvrinit.h"
#include "init.h"
#include "prn_data.h"
#include "setup.h"
#include "term.h"

int main() {
  // DigSim provides a FORTRAN architecture required to simulate
  // continuous systems described by sets of simultaneous first-order
  // differential equations.
  //
  // Variables:
  //     End_Of_Run - Flag set by user to stop the simulation [Logical]
  BOOL end_of_run , execute_run;
  // Open the input file
  FILE *input;
  input = fopen("input.dat","r");
  digsim_initialization(); // Global driver initialization
  driver_default_data();
  execute_run = input_data(input); // Get data for first run
  // If data is available, begin loop to execute simulation runs.
  while (execute_run) {
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
    // Advance the simulation states until termination.
    while (end_of_run != TRUE) {
      // Print the current values
      print_data();
      // Advance states to new time
      advance_states();
      // Calculate state derivatives at new time
      differential_equations();
      // Check the termination conditions
      end_of_run = termination_conditions();
    }
    // Setup for next run
    execute_run =input_data(input);
    // End of run execution loop
  }
  fclose(input);
}
