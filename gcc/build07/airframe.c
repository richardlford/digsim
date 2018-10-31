#include "airframe.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"

#define thetadot_b_cmd  real_array[300].r // ThetaDot_b_Cmd - Commanded attitude rate of missile [rad/sec]
#define thetadot_b      real_array[505].r // ThetaDot_b - Attitude rate ofmissile [rad/sec]

void airframe_response_data() {
  // This subroutine sets the default data for determining the airframe response
  // to a rate command.
  //
  // Inputs:
  //     None.
  //
  // Outputs:
  //     None.
  //
  // Internal variables and constants:
  //     None.
  //
  // Begin default data definitions:
  //     No default data required.
}

void airframe_response_init () {
  // This subroutine initializes the variables for determining the airframe
  // response to a rate command.
  //
  // Inputs:
  //     None.
  //
  // Outputs:
  //     None.
  // Begin math model initialization:
  // No initialization required.
}

void airframe_response() {
  // This subroutine determines the airframe response to a rate command.
  // Inputs:
  //  thetadot_b_cmd  real_array[300].r // ThetaDot_b_Cmd - Commanded attitude rate of missile [rad/sec]
  // Outputs:
  //   ThetaDot_b - Attitude rate ofmissile [rad/sec]
  // Internal variables and constants:
  //     None.
  // Begin math model:
  // Calculate airframe rate
  thetadot_b = thetadot_b_cmd;
}
