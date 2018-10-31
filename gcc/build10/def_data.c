#include "def_data.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include <math.h>
#include "actuator.h"
#include "actuator.h"
#include "aero.h"
#include "mass.h"
#include "kmtc.h"
#include "gyro.h"
#include "target.h"
#include "seeker.h"
#include "fcomp.h"

#define pi     real_array[50].r
#define rdtodg real_array[51].r
#define small  real_array[52].r

void default_data() {
  // This subroutine calls the routines responsible for setting
  // default data values. It also sets default system values for this
  // simulation.
  //
  // Inputs:
  //   None.
  //
  // Outputs:
  //   Dt      - Integration time step [sec]
  //   DtMax   - Maximum allowable Dt [sec]
  //   DtMin   - Minimum allowable Dt [sec]
  //   DtPrint - Print data logging interval [sec]
  //   Time0   - Initial time [sec]
  //   Tstop   - Simulation termination time [sec]
  // Set model independent data and reset time
  dt = 0.005;
  dtmax = 0.005;
  dtmin = 0.001;
  dtprint = 0.01;
  time0 = 0.0;
  tstop = 10.0;
  // Commonly used constants
  pi = 4.0*atan(1.0);
  rdtodg = 180.0/pi;
  small = 0.000001;
  // Call routines to set default data
  actuator_data();
  aerodynamics_data();
  mass_data();
  kinematics_data();
  gyro_data();
  target_data();
  seeker_data();
  flight_computer_data();
}
