#include "gyro.h"
#include "../driver/global.h"
#include "../driver/defstate.h"

#define p_g_meas real_array[200].r
#define q_g_meas real_array[201].r
#define r_g_meas real_array[202].r
#define p_b      real_array[509].r
#define q_b      real_array[511].r
#define r_b      real_array[513].r

void gyro_data() {
  // Angular rate sensors model default data
}
void gyro_init() {
  // Angular rate sensors model initialization
}
void gyro() {
  // This model simulates signals from three axis of ideal rate gyros.
  //
  // Inputs:
  //   P_b - Missile inertial angular velocity [rad/sec]
  //   Q_b
  //   R_b
  //
  // Outputs:
  //   P_g_Meas  - Measured angular velocity [rad/sec]
  //   R_g_Meas
  //   Q_g_Meas
  // Attitude rate signals
  p_g_meas = p_b;
  q_g_meas = q_b;
  r_g_meas = r_b;
}
