#include  "advance.h"
#include  "def_data.h"
#include  "diffeq.h"
#include  "init.h"
#include  "term.h"

int main(void) {
  // DigSim provides a FORTRAN architecture required to simulate
  // continuous systems described by sets of simultaneous first-order
  // differential equations.
  //
  // Variables:
  //     End Of Run - Flag set by user to stop the s{mulation [Logical]
  //
  // Exceptions to default type
  _Bool end_of_run;
  default_data();                            // Set the default data for this run
  initialization();                          // Setup & initialize the states
  differential_equations();                  // Determine the inital value for the state derivatives
  end_of_run = termination_conditions();     // Get the initial value for the run termination flag
  while (end_of_run != TRUE) {               // Advance the simulation states until termination
    advance_states();                        // Advance states to new time
    differential_equations();                // Calculate state derivatives at new time
    end_of_run = termination_conditions();   // Check for end of run
  }
}
