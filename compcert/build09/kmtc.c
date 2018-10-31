#include "kmtc.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/deriv.h"
#include "../driver/defstate.h"
#include "../driver/limit.h"
#include <math.h>

#define rdtodg        real_array[ 51].r
#define aaerox_bi_b   real_array[100].r
#define aaeroy_bi_b   real_array[101].r
#define aaeroz_bi_b   real_array[102].r
#define x_bi_i        real_array[500].r
#define xd_bi_i       real_array[501].r
#define xdd_bi_i      real_array[502].r
#define y_bi_i        real_array[503].r
#define yd_bi_i       real_array[504].r
#define ydd_bi_i      real_array[505].r
#define z_bi_i        real_array[506].r
#define zd_bi_i       real_array[507].r
#define zdd_bi_i      real_array[508].r
#define p_b           real_array[509].r
#define q_b           real_array[511].r
#define r_b           real_array[513].r
#define psi_b         real_array[515].r
#define theta_b       real_array[516].r
#define phi_b         real_array[517].r
#define q0_b          real_array[518].r
#define q0d_b         real_array[519].r
#define q1_b          real_array[520].r
#define q1d_b         real_array[521].r
#define q2_b          real_array[522].r
#define q2d_b         real_array[523].r
#define q3_b          real_array[524].r
#define q3d_b         real_array[525].r
#define tib11         real_array[526].r
#define tib12         real_array[527].r
#define tib13         real_array[528].r
#define tib21         real_array[529].r
#define tib22         real_array[530].r
#define tib23         real_array[531].r
#define tib31         real_array[532].r
#define tib32         real_array[533].r
#define tib33         real_array[534].r
#define psi_b_ic_dg   real_array[536].r
#define theta_b_ic_dg real_array[537].r
#define phi_b_ic_dg   real_array[538].r
#define p_b_ic_dg     real_array[539].r
#define q_b_ic_dg     real_array[540].r
#define r_b_ic_dg     real_array[541].r
#define xd_bi_i_ic    real_array[542].r
#define yd_bi_i_ic    real_array[543].r
#define zd_bi_i_ic    real_array[544].r
#define x_bi_i_ic     real_array[545].r
#define y_bi_i_ic     real_array[546].r
#define z_bi_i_ic     real_array[547].r
#define acc_gravity   real_array[548].r

void kinematics_data() {
  // Outputs:
  //   Acc_Gravity   - Acceleration due to gravity [m/sec**2]
  //   Psi_b_IC_dg   - Initial Euler angles [deg]
  //   Theta_b_IC_dg
  //   Phi_b_IC_dg
  //   P_b_IC_dg     - Initial missile rotation velocity [deg/sec]
  //   Q_b_IC_dg
  //   R_b_IC_dg
  //   Xd_bi_i_IC    - Initial missile velocity WRT ICS [m/sec]
  //   Yd_bi_i_IC
  //   Zd_bi_i_IC
  //   X_bi_i_IC     - Initial missile position [m]
  //   Y_bi_i_IC
  //   Z_bi_i_IC
  // Begin default data definitions:
  // Variables for determining initial attitude
  psi_b_ic_dg = 0.0;
  theta_b_ic_dg = 0.0;
  phi_b_ic_dg = 0.0;
  // Variables for determining initial angular velocity
  p_b_ic_dg = 0.0;
  q_b_ic_dg = 0.0;
  r_b_ic_dg = 0.0;
  // Variables for determining initial translational velocity of the BCS w.r.t.
  // the ICS
  xd_bi_i_ic = 100.0;
  yd_bi_i_ic = 0.0;
  zd_bi_i_ic = 0.0;
  // Variables for determining initial translational position of the BCS w.r.t.
  // the ICS
  x_bi_i_ic = 0.0;
  y_bi_i_ic = 0.0;
  z_bi_i_ic = -100.0;
  // Acceleration due to gravity
  acc_gravity = 9.88;
}

void kinematics_init() {
  // Missile kinematics model initialization; initializes quaternions from
  // initial missile Euler angles.
  //
  // Inputs:
  //   Psi_b_IC_dg   - Initial Euler angles [deg]
  //   Theta_b_IC_dg
  //   Phi_b_IC_dg
  //   P_b_IC_dg     - Initial missile rotational velocity [deg/sec]
  //   Q_b_IC_dg
  //   R_b_IC_dg
  //   Xd_bi_i_IC    - Initial missile velocity WRT ICS [m/sec]
  //   Yd_bi_i_IC
  //   Zd_bi_i_IC
  //   X_bi_i_IC     - Initial missile position [m]
  //   Y_bi_i_IC
  //   Z_bi_i_IC
  //
  // Outputs:
  //   P_b     - Missile inertial angular velocity [rad/sec]
  //   Q_b
  //   R_b
  //   Q0_b    - ICS to BCS quaternion [Real]
  //   Q1_b
  //   Q2_b
  //   Q3_b
  //   X_bi_i  - Missile terminal position WRT ICS [m]
  //   Y_bi_i
  //   Z_bi_i
  //   Xd_bi_i - Missile velocity in ICS [m/sec]
  //   Yd_bi_i
  //   Zd_bi_i
  //
  // Internal variables and constants:
  //   CPsiO2   - Cosines of half Euler angles [Real]
  //   CThetaO2
  //   CPhiO2
  //   RDTODG   - Radians to degrees conversion factor [deg/rad]
  //   SPsiO2   - Sines of half Euler angles [Real]
  //   SThetaO2
  //   SPhiO2
  //
  // Declare global common and assign variable locations
  REAL cpsio2,cthetao2,cphio2,spsio2,sthetao2,sphio2;

  // Assign state and state derivative pointers
  define_real_state(500,501);
  define_real_state(501,502);
  define_real_state(503,504);
  define_real_state(504,505);
  define_real_state(506,507);
  define_real_state(507,508);
  define_real_state(509,510);
  define_real_state(511,512);
  define_real_state(513,514);
  define_real_state(518,519);
  define_real_state(520,521);
  define_real_state(522,523);
  define_real_state(524,525);
  // Begin math model initialization
  // Initial Euler angles: (BCS) = [ROLL][PITCH][YAW](ICS)
  psi_b = psi_b_ic_dg/rdtodg;
  theta_b = theta_b_ic_dg/rdtodg;
  phi_b = phi_b_ic_dg/rdtodg;
  // Initial angular velocity of BCS w.r.t. ICS, expressed in BCS
  p_b = p_b_ic_dg/rdtodg;
  q_b = q_b_ic_dg/rdtodg;
  r_b = r_b_ic_dg/rdtodg;
  // Initial translational position of the BCS w.r.t. the ICS
  x_bi_i = x_bi_i_ic;
  y_bi_i = y_bi_i_ic;
  z_bi_i = z_bi_i_ic;
  // Initial translational velocity of the BCS w.r.t. to the ICS
  xd_bi_i = xd_bi_i_ic;
  yd_bi_i = yd_bi_i_ic;
  zd_bi_i = zd_bi_i_ic;
  // Trig functions of half Euler angles
  cpsio2 = cos(0.5*psi_b);
  cthetao2 = cos(0.5*theta_b);
  cphio2 = cos(0.5*phi_b);
  spsio2 = sin(0.5*psi_b);
  sthetao2 = sin(0.5*theta_b);
  sphio2 = sin(0.5*phi_b);
  // Initial quaternion values
  q0_b = cpsio2*cthetao2*cphio2 + spsio2*sthetao2*sphio2;
  q1_b = cpsio2*cthetao2*sphio2 - spsio2*sthetao2*cphio2;
  q2_b = cpsio2*sthetao2*cphio2 + spsio2*cthetao2*sphio2;
  q3_b = spsio2*cthetao2*cphio2 - cpsio2*sthetao2*sphio2;
}

void kinematics() {
  // This module evaluates the translational and rotational derivatives which
  // describe the missile's motion WRT an earth fixed coordinate system for
  // a non-rotating earth.
  //
  // Inputs:
  //   AaeroX_bi_b - Accelerations due to aero in BCS [m/sec**2]
  //   AaeroY_bi_b
  //   AaeroZ_bi_b
  //   Acc_Gravity - Acceleration due to gravity [m/sec**2]
  //   P_b         - Missile inertial angular velocity [rad/sec]
  //   Q_b
  //   R_b
  //   Q0_b        - ICS to BCS quaternion [Real]
  //   Q1_b
  //   Q2_b
  //   Q3_b
  //   Tibij       - ICS to BCS transformation matrix [Real]
  //
  // Outputs:
  //   Q0_b     - ICS to BCS quaternion [Real]
  //   Q1_b
  //   Q2_b
  //   Q3_b
  //   Qd0_b    - ICS to BCS quaternion derivatives [Real]
  //   Qd1_b
  //   Qd2_b
  //   Qd3_b
  //   Psi_b    - Missile Euler angles [rad]
  //   Theta_b
  //   Phi_b
  //   Xdd_bi_i - Missile acceleration WRT ICS in ICS [m/sec**2]
  //   Ydd_bi_i
  //   Zdd_bi_i
  //
  // Internal variables and constants:
  //   Qmag     - Magnitude of ICS to BCS quaternion [Real]
  //   RDTODG   - Radians to degrees conversion factor [deg/rad]
  //   STheta_b - Sine of pitch Euler angle [Real]
  //
  // Declare global common and assign variable locations
  REAL stheta_b,qmag;
  // Begin math model:
  // Evaluate Euler angles
  stheta_b = limit(-tib13,-1.0,1.0);
  theta_b = asin(stheta_b);
  psi_b = atan2(tib12,tib11);
  phi_b = atan2(tib23,tib33);
  // Missile velocity rate in ICS WRT ICS
  xdd_bi_i = tib11*aaerox_bi_b + tib21*aaeroy_bi_b + tib31*aaeroz_bi_b;
  ydd_bi_i = tib12*aaerox_bi_b + tib22*aaeroy_bi_b + tib32*aaeroz_bi_b;
  zdd_bi_i = tib13*aaerox_bi_b + tib23*aaeroy_bi_b + tib33*aaeroz_bi_b + acc_gravity;
  // Quaternion constraint equation
  qmag = sqrt(q0_b*q0_b+q1_b*q1_b+q2_b*q2_b+q3_b*q3_b);
  q0_b = q0_b/qmag;
  q1_b = q1_b/qmag;
  q2_b = q2_b/qmag;
  q3_b = q3_b/qmag;
  // Quaternion derivative
  q0d_b = -0.5*(q1_b*p_b+q2_b*q_b+q3_b*r_b);
  q1d_b =  0.5*(q0_b*p_b+q2_b*r_b-q3_b*q_b);
  q2d_b =  0.5*(q0_b*q_b+q3_b*p_b-q1_b*r_b);
  q3d_b =  0.5*(q0_b*r_b+q1_b*q_b-q2_b*p_b);
}
