#include "init.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/deriv.h"
#include "../driver/defstate.h"

#define x_ic  real_array[12].r // Initial position of ball [m]
#define xd_ic real_array[13].r // Initial velocity of ball [m/sec]
#define x     real_array[14].r // Position of ball [m]
#define xd    real_array[15].r // Velocity of ball [m/sec]

void initialization(void) {
  // This routine initializes state values and state counters and pointers.
  time = time0; // Set initial time
  // Setup integrators
  define_real_state(14,15);
  define_real_state(15,16);
  // Initialize states
  x = x_ic;
  xd = xd_ic;
}
