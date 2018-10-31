#include "seeker.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#define x_bi_i   real_array[500].r // X position of BCS w.r.t. ICS, expressed in the ICS [m]
#define z_bi_i   real_array[501].r // Z position of BCS w.r.t. ICS, expressed in the ICS [m]
#define xd_bi_i  real_array[502].r // X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
#define zd_bi_i  real_array[503].r // Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
#define q_s      real_array[600].r // Q_s - LOS rate [rad/sec]
#define q_s_meas real_array[601].r // Q_s_Meas - Measured LOS rate [rad/sec]

void seeker_data(void) {
  // This subroutine sets the default data for the seeker model.
  // Inputs:
  //     None.
  // Outputs:
  //     None.
  // Internal variables and constants
  //     None.
}

void seeker_init() {
  //       This subroutine initializes the variables for the seeker model.
  //       Inputs:
  //           None.
  //       Outputs:
  //           None.
}

void seeker(void) {
  // This subroutine models a perfect one-axis seeker/tracker.
  //
  // Inputs:
  //     X_bi_i - X position of BCS w.r.t. ICS, expressed in the ICS [m]
  //     Xd_bi_i - X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //     Z_bi_i - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
  //     Zd_bi_i - Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //
  // Outputs:
  //     Q_s - LOS rate [rad/sec]
  //     Q_s_Meas - Measured LOS rate [rad/sec]
  // Begin math model:
  // Calculate true LOS rate
  q_s = (z_bi_i*xd_bi_i - x_bi_i*zd_bi_i)/(x_bi_i*x_bi_i + z_bi_i*z_bi_i);
  // Calculate measured LOS rate
  q_s_meas = q_s;
}
