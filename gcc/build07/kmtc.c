#include "kmtc.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/deriv.h"
#include "../driver/defstate.h"
#include <math.h>

#define rdtodg        real_array[50].r  // Radians to degrees conversion factor [deg/rad]
#define x_bi_i        real_array[500].r // X position of BCS w.r.t. ICS, expressed in the ICS [m]
#define z_bi_i        real_array[501].r // Z position of BCS w.r.t. ICS, expressed in the ICS [m]
#define xd_bi_i       real_array[502].r // X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
#define zd_bi_i       real_array[503].r // Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
#define theta_b       real_array[504].r // Attitude of missile [rad]
#define x_bi_i_ic     real_array[506].r // Initial X position of BCS w.r t ICS, expressed in the ICS [m]
#define z_bi_i_ic     real_array[507].r // Initial Z position of BCS w.r t ICS, expressed in the ICS [m]
#define theta_b_ic_dg real_array[508].r // Initial attitude of missile [deg]
#define velocity      real_array[510].r // Velocity of missile [m/sec]

void kinematics_data() {
  // This subroutine sets the default data for the kinematics routine.
  //
  // Inputs:
  //     None.
  //
  // Outputs:
  //     Theta_b_IC_dg - Initial attitude of missile [deg]
  //     Velocity - Velocity of missile [m/sec]
  //     X_bi_i_IC - Initial X position of BCS w.r t ICS, expressed in the ICS [m]
  //     Z_bi_i_IC - Initial Z position of BCS w.r t ICS, expressed in the ICS [m]
  // Begin default data definitions:
  theta_b_ic_dg = 0.0;
  velocity = 100.0;
  x_bi_i_ic = -500.0;
  z_bi_i_ic = -100.0;
  }

void kinematics_init() {
  // This subroutine initializes the variables for the kinematics routine
  //
  // Inputs:
  //     RDTODG - Radians to degrees conversion factor [deg/rad]
  //     Theta_b_IC_dg - Initial attitude of missile [deg]
  //     X_bi_i_IC - Initial X position of BCS w.r t ICS, expressed in the ICS [m]
  //     Z_pi_i_IC - Initial Z position of BCS w.r t ICS, expressed in the ICS [m]
  //
  // Outputs:
  //     Theta_b - Attitude of missile [rad]
  //     X_bi_i - X position of BCS w.r.t. ICS, expressed in the ICS [m]
  //     Z_bi_i - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
  // Begin math model initialization:
  // Define the states
  define_real_state(500,502);
  define_real_state(501,503);
  define_real_state(504,505);
  // Set the initial conditions
  x_bi_i = x_bi_i_ic;
  z_bi_i = z_bi_i_ic;
  theta_b = theta_b_ic_dg/rdtodg;
}

void kinematics() {
  // This subroutine determines the kinematics state derivatives
  //
  // Inputs:
  //     Theta_b - Attitude of missile [rad]
  //     Velocity - Velocity of missile [m/sec]
  //
  // Outputs:
  //     Xd_bi_i - X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //     Zd_bi_i - Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //
  // Internal variables and constants:
  //
  // Declare global common and assign variable locations
  // Begin math model:
  // Calculate velocities (attitude rate comes directly from airframe response)
  xd_bi_i =  velocity*cos(theta_b);
  zd_bi_i = -velocity*sin(theta_b);
}
