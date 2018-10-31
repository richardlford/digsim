#include "def_data.h"
#include "kmtc.h"
#include "seeker.h"
#include "fcomp.h"
#include "airframe.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include <math.h>

void default_data() {
  // This subroutine sets the default data values.
  //
  // Inputs:
  //     RDTODG  - Radians to degrees conversion factor [deg/rad]
  //
  // Outputs:
  //     DtMax   - Maximum allowable simulation time step [sec]
  //     DtMin   - Minimum allowable simulation time step [sec]
  //     DtPrint - Time step between printing data [sec]
  //     Time0   - Initial time [sec]
  //     Tstop   - Simulation stop time [sec]
#define rdtodg real_array[50].r // Radians to degrees conversion factor [deg/rad]
  // Internal variables and constants:
  //     None.
  //
  // Declare global common
  // Set model independent data and reset time
  dtmax   = 0.005;
  dtmin   = 0.001;
  dtprint = 0.01;
  time0   = 0.0;
  tstop   = 10.0;
  rdtodg  = 180.0/M_PI;
  // Set the default data for the simulation
  kinematics_data();
  seeker_data();
  flight_computer_data();
  airframe_response_data();
}
