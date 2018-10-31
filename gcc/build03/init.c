#include "init.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/deriv.h"
#include "../driver/defstate.h"
#define x_ic  real_array[14].r  // Initial position of suspended mass [m]
#define xd_ic real_array[15].r  // Initial velocity of suspended mass [m/sec]
#define x     real_array[16].r  // Position of suspended mass [m]
#define xd    real_array[17].r  // Velocity of suspended mass [m/sec]
void initialization(void) {
  // This routine initializes state values and state counters and pointers.
  time = time0; // Set initial time
  // Setup integrators
  define_real_state(16,17);
  define_real_state(17,18);
  // Initialize states
  x = x_ic;
  xd = xd_ic;
}
