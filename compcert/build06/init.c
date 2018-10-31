#include "init.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/deriv.h"
#include "../driver/defstate.h"

#define rdtodg      real_array[10].r  // Radians to degrees conversion factor [deg/rad]
#define theta_ic_dg real_array[12].r  // Initial attitude of missile [deg]
#define x_ic        real_array[14].r  // Initial X position of missile [m]
#define z_ic        real_array[15].r  // Initial Z position of missile [m]
#define x           real_array[16].r  // X position of missile [m]
#define z           real_array[17].r  // Z position of missile [m]
#define theta       real_array[19].r  // Attitude of missile [rad]

void initialization(void) {
  // This routine initializes state values and state counters and pointers.

  // Set initial time
  time = time0;
  // Setup integrators
  define_real_state(16,19);
  define_real_state(17,20);
  define_real_state(18,21);
  // Initialize states
  x = x_ic;
  z = z_ic;
  theta = theta_ic_dg/rdtodg;
}
