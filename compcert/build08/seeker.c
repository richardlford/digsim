#include "seeker.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"

#define x_bi_i   real_array[500].r
#define z_bi_i   real_array[503].r
#define xd_bi_i  real_array[501].r
#define zd_bi_i  real_array[504].r
#define q_s      real_array[600].r
#define q_s_meas real_array[601].r
#define x_bt_i   real_array[602].r
#define z_bt_i   real_array[603].r
#define xd_bt_i  real_array[604].r
#define zd_bt_i  real_array[605].r
#define x_ti_i   real_array[700].r
#define xd_ti_i  real_array[701].r
#define z_ti_i   real_array[702].r
#define zd_ti_i  real_array[703].r

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
  // Inputs:
  //   X_bi_i  - X position of the BCS w.r.t. ICS, expressed in the ICS [m]
  //   X_ti_i  - X position of the target, w.r.t. ICS in ICS[m]
  //   Xd_bi_i - X velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //   Xd_ti_i - X velocity of the target, w.r.t. ICS in ICS [m/sec]
  //   Z_bi_i  - Z position of the BCS w.r.t. ICS, expressed in the ICS [m]
  //   Z_ti_i  - Z position of the target, w.r.t. ICS in ICS [m]
  //   Zd_bi_i - Z velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //   Zd_ti_i - Z velocity of the target, w.r.t. ICS in ICS [m/sec]
  // Outputs:
  //   Q_s             - LOS rate [rad/sec]
  //   Q_s_Meas        - Measured LOS rate [rad/sec]
  //   X_bt_i          - X position of the BCS w.r.t. ICS, expressed in the ICS [m]
  //   Xd_bt_i         - X velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //   Z_bt_i          - Z position of the BCS w.r.t. ICS, expressed in the ICS [m]
  //   Zd_bt_i         - Z velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
  // Begin math model:
  // Target relative position and velocity in ICS
  x_bt_i = x_bi_i - x_ti_i;
  z_bt_i = z_bi_i - z_ti_i;
  xd_bt_i = xd_bi_i - xd_ti_i;
  zd_bt_i = zd_bi_i - zd_ti_i;
  // Calculate true LOS rate
  q_s = (z_bt_i*xd_bt_i - x_bt_i*zd_bt_i)/(x_bt_i*x_bt_i + z_bt_i*z_bt_i);
  // Calculate measured L08 rate
  q_s_meas = q_s;
}
