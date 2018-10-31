#include "def_data.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#define damping_coefficient real_array[10].r  // Damping force per velocity [N/m/s]
#define gravity             real_array[11].r  // Acceleration due to gravity [m/sec**2]
#define mass                real_array[12].r  // Mass suspended from spring [Kg]
#define spring_coefficient  real_array[13].r  // Restoring force per position [N/m]
#define x_ic                real_array[14].r  // Initial position of suspended mass [m]
#define xd_ic               real_array[15].r  // Initial velocity of suspended mass [m/sec]
void default_data(void) {
  // Set model independent data and reset time
  dt    = 0.01;
  time0 = 0.0;
  tstop = 2.5;
  // Set the default data for the simulation
  damping_coefficient = 8.88;
  gravity             = 9.88;
  mass                = 1.0;
  spring_coefficient  = 39.47;
  x_ic                = 0.0;
  xd_ic               = 0.0;
}
