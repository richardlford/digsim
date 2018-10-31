#include "kmtc.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/deriv.h"
#include "../driver/defstate.h"
#include <math.h>

#define aaerox_bi_b   real_array[100].r
#define aaeroz_bi_b   real_array[101].r
#define xdd_bi_i      real_array[502].r
#define zdd_bi_i      real_array[505].r
#define theta_b       real_array[506].r
#define gravity       real_array[515].r
#define rdtodg        real_array[50].r
#define x_bi_i        real_array[500].r
#define xd_bi_i       real_array[501].r
#define z_bi_i        real_array[503].r
#define zd_bi_i       real_array[504].r
#define q_b           real_array[507].r
#define x_bi_i_ic     real_array[509].r
#define xd_bi_i_ic    real_array[510].r
#define z_bi_i_ic     real_array[511].r
#define zd_bi_i_ic    real_array[512].r
#define theta_b_ic_dg real_array[513].r
#define q_b_ic_dg     real_array[514].r

void kinematics_data() {
  // This subroutine sets the Default data for the kinematics routine.
  //
  // Inputs:
  //     None.
  //
  // Outputs:
  //     Gravity       - Acceleration due to gravity [m/sec**2]
  //     Theta_b_IC_dg - Initial attitude of missile [deg]
  //     Q_b_IC_dg     - Initial attitude rate of missile [deg/sec]
  //     X_bi_i_IC     - Initial X position of BCS w.r.t. ICS, expressed in the ICS [m]
  //     Xd_bi_i_IC    - Initial X velocity of BCS w.r.t. ICS, expressed ICS [m/sec]
  //     Z_bi_i_IC     - Initial Z position of BCS w.r.t. ICS, expressed ICS [m]
  //     Zd_bi_i_IC    - Initial Z velocity of BCS w.r.t. ICS, expressed ICS [m/sec]
  // Begin default data definitions:
  theta_b_ic_dg = 0.0;
  q_b_ic_dg = 0.0;
  x_bi_i_ic = -500.0;
  z_bi_i_ic = -100.0;
  xd_bi_i_ic = 100.0;
  zd_bi_i_ic = 0.0;
  gravity = 9.88;
}

void kinematics_init() {
  // Inputs:
  //     RDTODG        - Radians to degrees conversion factor [deg/rad]
  //     Theta_b_IC_dg - Initial attitude of missile [deg]
  //     Q_b_IC_dg     - Initial attitude rate of missile [deg/sec]
  //     X_bi_i_IC     - Initial X position of BCS w.r.t. ICS, expressed in the
  //                     ICS [m]
  //     Xd_bi_i_IC    - Initial X velocity of BCS w.r.t. ICS, expressed
  //                     ICS [m/sec]
  //     Z_bi_i_IC     - Initial Z position of BCS w.r.t. ICS, expressed
  //                     ICS [m]
  //     Zd_bi_i_IC    - Initial Z velocity of BCS w.r.t. ICS, expressed
  //                     ICS [m/sec]
  //
  // Outputs:
  //     Theta_b       - Attitude of missile [rad]
  //     Q_b           - Attitude rate of missile [rad/sec]
  //     X_bi_i        - X position of BCS w.r.t. ICS, expressed in the ICS [m]
  //     Xd_bi_i       - X velocity of BCS w.r.t. ICS, expressed ICS [m/sec]
  //     Z_bi_i        - Z position of BCS w.r.t. ICS, expressed ICS [m]
  //     Zd_bi_i       - Z velocity of BCS w.r.t. ICS, expressed ICS [m/sec]
  // Begin math model initialization:
  // Define the states
  define_real_state(500,501);
  define_real_state(501,502);
  define_real_state(503,504);
  define_real_state(504,505);
  define_real_state(506,507);
  define_real_state(507,508);
  // Set the initial conditions
  x_bi_i  = x_bi_i_ic;
  z_bi_i  = z_bi_i_ic;
  xd_bi_i = xd_bi_i_ic;
  zd_bi_i = zd_bi_i_ic;
  theta_b = theta_b_ic_dg/rdtodg;
  q_b = q_b_ic_dg/rdtodg;
}

void kinematics() {
  // This subroutine determined the kinematic state derivatives.
  //
  // Inputs:
  //     AaeroX_bi_b - Accelerations due to aero in BCS [mXsec**2)
  //     AaeroZ_hi_b
  //     Gravity - Acceleration due to gravity [m/sec**2]
  //     Theta_b - Attitude of missile [rad]
  //
  // Outputs:
  //     Xdd_bi_i - X acceleration of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //     Zdd_bi_i - Z acceleration of BCS w.r.t. ICS expressed in the ICS [m/sec]
  // Internal variables and constants:
  //
  // Begin math model:
  // Calculate velocities (attitude rate comes directly from airframe response)
  xdd_bi_i =  aaerox_bi_b*cos(theta_b) + aaeroz_bi_b*sin(theta_b);
  zdd_bi_i = -aaerox_bi_b*sin(theta_b) + aaeroz_bi_b*cos(theta_b)+ gravity;
}
