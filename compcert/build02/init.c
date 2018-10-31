#include "global.h"
#include "sysvars.h"
#include "deriv.h"
void initialization() {
  // This routine initializes state values and state counters and
  // pointers.
  //
  // Inputs:
  //    Time0 - Initial time [sec]
  //
  // Outputs:
  //    Time  - Simulation time [sec]
  //    X     - Position of suspended mass [m]
  //    X_IC  - Initial position of suspended mass [m]
  //    Xd    - Velocity of suspended mass [m/sec]
  //    Xd_IC - Initial velocity of suspended mass [m/sec]
  //
  // Internal variables and constants:
  //    None.
  //
  // Declare global common and assign variable locations
  #define x_ic      real_array[14].r
  #define xd_ic     real_array[15].r
  #define x         real_array[16].r
  #define xd        real_array[17].r
  // Set initial time
  time = time0;
  //  Setup integrators
  ndes_real = 2;
  ix_real   [0] = 16;
  ixdot_real[0] = 17;
  ix_real   [1] = 17;
  ixdot_real[1] = 18;
  // Initialize states
  x  = x_ic;
  xd = xd_ic;
}
