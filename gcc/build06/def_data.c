#include "def_data.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include <math.h>

#define rdtodg         real_array[10].r  // Radians to degrees conversion factor [deg/rad]
#define theta_ic_dg    real_array[12].r  // Initial attitude of missile [deg]
#define velocity       real_array[13].r  // Velocity of missile [m/sec]
#define x_ic           real_array[14].r  // Initial X position of missile [m]
#define z_ic           real_array[15].r  // Initial Z position of missile [m]
#define guidance_gain  real_array[25].r  // Commanded angular rate per measured LOS rate [Real]

void default_data(void) {
  // Set model independent data and reset time
  dtmax   = 0.005;
  dtmin   = 0.001;
  dtprint = 0.01;
  time0   = 0.0;
  tstop   = 10.0;
  rdtodg  = 180.0/M_PI;
  // Set the default data for the simulation
  theta_ic_dg = 0.0;
  velocity    = 100.0;
  x_ic        = -500.0;
  z_ic        = -100.0;
  guidance_gain = 3.0;
}
