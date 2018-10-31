#include "def_data.h"
#include "kmtc.h"
#include "seeker.h"
#include "fcomp.h"
#include "airframe.h"
#include "target.h"
#include "gyro.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include <math.h>

#define pi     real_array[50].r
#define rdtodg real_array[51].r
#define small  real_array[52].r

void default_data() {
  // This subroutine calls the routines responsible for setting default data
  // values.  It also sets default system values for this simulation.

  // Inputs:
  //     RDTODG  - Radians to degrees conversion factor [deg/rad]
  //
  // Outputs:
  //     DtMax   - Maximum allowable simulation time step [sec]
  //     DtMin   - Minimum allowable simulation time step [sec]
  //     DtPrint - Time step between printing data [sec]
  //     Time0   - Initial time [sec]
  //     Tstop   - Simulation stop time [sec]
  //     rdtodg  - Radians to degrees conversion factor [deg/rad]
  // Internal variables and constants:
  //     None.
  //
  // Declare global common
  // Set model independent data and reset time
  dt      = 0.005;
  dtmax   = 0.005;
  dtmin   = 0.001;
  dtprint = 0.01;
  time0   = 0.0;
  tstop   = 10.0;
  pi      = M_PI;
  rdtodg  = 180.0/M_PI;
  small = 0.000001;
  // Set the default data for the simulation
  airframe_response_data();
  kinematics_data();
  gyro_data();
  target_data();
  seeker_data();
  flight_computer_data();
}
