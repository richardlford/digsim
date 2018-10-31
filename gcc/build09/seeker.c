#include "seeker.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/limit.h"

#define small       real_array[52].r
#define x_bi_i      real_array[500].r
#define xd_bi_i     real_array[501].r
#define y_bi_i      real_array[503].r
#define yd_bi_i     real_array[504].r
#define z_bi_i      real_array[506].r
#define zd_bi_i     real_array[507].r
#define tib21       real_array[529].r
#define tib22       real_array[530].r
#define tib23       real_array[531].r
#define tib31       real_array[532].r
#define tib32       real_array[533].r
#define tib33       real_array[534].r
#define q_si_b_meas real_array[600].r
#define r_si_b_meas real_array[601].r
#define p_s         real_array[602].r
#define q_s         real_array[603].r
#define r_s         real_array[604].r
#define range_tb_sq real_array[605].r
#define x_tb_i      real_array[606].r
#define xd_tb_i     real_array[607].r
#define y_tb_i      real_array[608].r
#define yd_tb_i     real_array[609].r
#define z_tb_i      real_array[610].r
#define zd_tb_i     real_array[611].r
#define x_ti_i      real_array[700].r
#define xd_ti_i     real_array[701].r
#define y_ti_i      real_array[702].r
#define yd_ti_i     real_array[703].r
#define z_ti_i      real_array[704].r
#define zd_ti_i     real_array[705].r

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
  // Perfect seeker/tracker model
  //
  // Inputs:
  //   Tibij   - ICS to BCS transformation matrix [Real]
  //   X_bi_i  - Missile terminal position WRT ICS [m]
  //   Y_bi_i
  //   Z_bi_i
  //   X_ti_i  - Position of target WRT ICS in ICS [m]
  //   Y_ti_i
  //   Z_ti_i
  //   Xd_bi_i - Missile velocity in ICS [m/sec]
  //   Yd_bi_i
  //   Zd_bi_i
  //   Xd_ti_i - Velocity of target WRT ICS in ICS [m/sec]
  //   Yd_ti_i
  //   Zd_ti_i
  //
  // Outputs:
  //   P_s         - Inertially referenced LOS rates, expressed in the ICS
  //   Q_s           [rad/sec]
  //   R_s
  //   Q_si_b_Meas - LOS rates expressed in the BCS [rad/sec]
  //   R_si_b_Meas
  //   X_tb_i      - Position of the target w.r.t. the BCS, expressed in the ICS
  //   Y_tb_i        [m]
  //   Z_tb_i
  //   Xd_tb_i     - Velocity of the target w.r.t. the BCS, expressed in the ICS
  //   Yd_tb_i       [m/sec]
  //   Zd_tb_i
  //
  // Internal variables and constants:
  //   SMALL       - Arbitrarily small number [Real]
  //   Range_tb_Sq - Target to body range, squared & limited [m**2]
  // Begin math model:
  // Target relative position and velocity in ICS
  x_tb_i = x_ti_i - x_bi_i;
  y_tb_i = y_ti_i - y_bi_i;
  z_tb_i = z_ti_i - z_bi_i;
  xd_tb_i = xd_ti_i - xd_bi_i;
  yd_tb_i = yd_ti_i - yd_bi_i;
  zd_tb_i = zd_ti_i - zd_bi_i;
  // Missile/target range, squared & limited
  range_tb_sq = MAX(small, (x_tb_i*x_tb_i+y_tb_i*y_tb_i+z_tb_i*z_tb_i));
  // Beam rotation rate WRT inertial space in ICS
  p_s = (y_tb_i*zd_tb_i-z_tb_i*yd_tb_i)/range_tb_sq;
  q_s = (z_tb_i*xd_tb_i-x_tb_i*zd_tb_i)/range_tb_sq;
  r_s = (x_tb_i*yd_tb_i-y_tb_i*xd_tb_i)/range_tb_sq;
  // Beam rotation rate WRT inertial space in BCS
  q_si_b_meas = tib21*p_s + tib22*q_s + tib23*r_s;
  r_si_b_meas = tib31*p_s + tib32*q_s + tib33*r_s;
}
