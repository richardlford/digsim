#include "fcomp.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"

#define guidance_gain  real_array[301].r // Commanded angular rate per measured LOS rate [Real]
#define q_b_cmd        real_array[300].r //  Comanded attitude rate of missile [rad/sec]
#define q_s_meas       real_array[601].r // Measured LOS rate [rad/sec]

void flight_computer_data() {
  // This subroutine sets the default data for the flight computer.
  //
  // Inputs:
  //     None.
  //
  // Outputs:
  //     Guidance_Gain - Commanded angular rate per measured LOS rate [Real]
  //  Internal variables and constants:
  //      None.
  //
  //  Declare global common and assign variable locations

  // Begin default data definitions:
  guidance_gain = 3.0;
}

void flight_computer_init() {
  // This subroutine initializes the variables for the flight computer.
  //
  // Inputs:
  //     None.
  // Outputs:
  //     None.
   // Internal variables and constants:
  //     None.
  //
  // Begin math model initialization:
  //
  // No initialization required.
}

void flight_computer(void) {
  // This subroutine determines the commanded airframe rate.
  // Inputs:
  //     Guidance_Gain - Commanded angular rate per measured LOS rate [Real]
  //     Q_s_Meas - Measured LOS rate [rad/sec]
  // Outputs:
  //     Q_b_Cmd - Comanded attitude rate of missile [rad/sec]
  // Internal variables and constants:
  //     None.
  // Begin math model:
  // Calculate guidance command
  q_b_cmd = guidance_gain*q_s_meas;
}
