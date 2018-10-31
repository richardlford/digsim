#include "init.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "kmtc.h"
#include "seeker.h"
#include "fcomp.h"
#include "airframe.h"

void initialization() {
  // This routine calls the routines to initialize state values, state counters
  // and pointers.
  //
  // Inputs:
  //     Time0 - Initial time [sec] ' '
  //
  // Outputs:
  //     Time - Simulation time [sec]
  // Set initial time
  time = time0;
  // Initialize models
  kinematics_init();
  seeker_init();
  flight_computer_init();
  airframe_response_init();
}
