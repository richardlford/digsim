#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/deriv.h"
void advance_states() {
  // This subroutine solves differential equation initial value problems; note
  // that the initial derivatives are assumed to have been evaluated prior to the
  // subroutine call.

  // Use Euler method to advance states one time step
  for (int i = 0; i <= ndes_real; i++)
    real_array[ix_real[i]].r += real_array[ixdot_real[i]].r*dt;
  // Advance time to match states
  time = time + dt;
}
