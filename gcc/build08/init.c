#include "init.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "airframe.h"
#include "fcomp.h"
#include "kmtc.h"
#include "seeker.h"
#include "target.h"

void initialization() {
  // This routine calls the routines to initialize state values, state counters
  // and pointers.
  // Inputs:
  //     Time0 - Initial time [sec] ' '
  // Outputs:
  //     Time - Simulation time [sec]
  // Set initial time
  time = time0;
  // Initialize models
  airframe_response_init();
  target_init();
  seeker_init();
  flight_computer_init();
  kinematics_init();
}

