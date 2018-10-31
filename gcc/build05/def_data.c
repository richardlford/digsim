#include "def_data.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"

#define coef_of_restitution real_array[10].r // Coefficent of restitution [Real]
#define gravity             real_array[11].r // Acceleration due to gravity [m/sec**2]
#define x_ic                real_array[12].r // Initial position of ball [m]
#define xd_ic               real_array[13].r // Initial velocity of ball [m/sec]

void default_data(void) {
  // Set model independent data and reset time
  dtmax = 0.005;
  dtmin = 0.001;
  dtprint = 0.01;
  time0 = 0.0;
  tstop = 10.0;
  // Set the default data for the simulation
  coef_of_restitution = 0.8;
  gravity = 9.88;
  x_ic = 10.0;
  xd_ic = 0.0;
}
