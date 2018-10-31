#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "init.h"
#include "airframe.h"
#include "kmtc.h"
#include "gyro.h"
#include "target.h"
#include "seeker.h"
#include "fcomp.h"

void initialization() {
  // This routine initializes state values and state counters and pointers;
  // then calls the routines to initialize the various models.
  //
  // Inputs:
  //   Time0  - Initial time [sec]
  //
  // Outputs:
  //   Time   - Simulation time [sec]

  // Set initial time
  time = time0;
  // Initialize models
  airframe_response_init();
  kinematics_init();
  gyro_init();
  target_init();
  seeker_init();
  flight_computer_init();
}
