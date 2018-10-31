#include "global.h"
#include "sysvars.h"
#include "deriv.h"
void advance_states() {
  // This subroutine solves differential equation initial value problems; note
  // that the initial derivatives are assumed to have been evaluated prior to
  // the subroutine call.
  //
  // Inputs:
  //     Dt         - Integration step size [sec]
  //     Ix_Real    - Real_Array locations of states [Integer Array]
  //     Ndes_Real  - Number of real states [Integer]
  //     Real_Array - The communications array for real variables [Real Array]
  //
  // Outputs:
  //    Real_Array - The communications array for real variables [Real Array)
  //    Time       - Simulation time [sec]
  //
  // Internal variables and constants:
  //    I - Loop index [Integer]
  //
  // Use Euler method to advance states one time step
  for (int i = 0; i < ndes_real; i++)
    real_array[ix_real[i]].r = real_array[ix_real[i]].r + real_array[ixdot_real[i]].r*dt;

  // Advance time to match states
  time = time + dt;
}
