#include "airframe.h"
#include "../driver/limit.h"
#include <math.h>

#define small          real_array[52].r
#define pi             real_array[50].r
#define rdtodg         real_array[51].r
#define aaerox_bi_b    real_array[100].r
#define aaeroy_bi_b    real_array[101].r
#define aaeroz_bi_b    real_array[102].r
#define alpha          real_array[103].r
#define alpha_ref      real_array[104].r
#define beta           real_array[105].r
#define beta_ref       real_array[106].r
#define drag_per_velsq real_array[107].r
#define rmin_xy        real_array[108].r
#define rmin_xz        real_array[109].r
#define tau_p          real_array[110].r
#define omega0_q       real_array[111].r
#define omega0_r       real_array[112].r
#define zeta_q         real_array[113].r
#define zeta_r         real_array[114].r
#define alpha_ref_dg   real_array[115].r
#define beta_ref_dg    real_array[116].r
#define freq0_q        real_array[117].r
#define freq0_r        real_array[118].r
#define p_b_cmd        real_array[300].r
#define q_b_cmd        real_array[301].r
#define r_b_cmd        real_array[302].r
#define xd_bi_i        real_array[501].r
#define yd_bi_i        real_array[504].r
#define zd_bi_i        real_array[507].r
#define p_b            real_array[509].r
#define pd_b           real_array[510].r
#define q_b            real_array[511].r
#define qd_b           real_array[512].r
#define r_b            real_array[513].r
#define rd_b           real_array[514].r
#define q0_b           real_array[518].r
#define q1_b           real_array[520].r
#define q2_b           real_array[522].r
#define q3_b           real_array[524].r
#define tib11          real_array[526].r
#define tib12          real_array[527].r
#define tib13          real_array[528].r
#define tib21          real_array[529].r
#define tib22          real_array[530].r
#define tib23          real_array[531].r
#define tib31          real_array[532].r
#define tib32          real_array[533].r
#define tib33          real_array[534].r

void airframe_response_data() {
  // Airframe response model default data
  //
  // Inputs:
  //   None.
  //
  // Outputs:
  //   Alpha_Ref_dg   - Reference alpha (deg)
  //   Beta_Ref_dg    - Reference beta (deg)
  //   Drag_Per_VelSq - Base axial drag coefficient [Real]
  //   Freq0_q        - Natural frequency for pitch channel [Hz]
  //   Freq0_r        - Natural frequency for yaw channel [Hz]
  //   Rmin_xy        - Minimum turning radius in X-Y plane (BCS) [m]
  //   Rmin_xz        - Minimum turning radius in X-Z plane (BCS) [m]
  //   Tau_p          - Time constant for roll channel [sec]
  //   Zeta_q         - Damping ratio for pitch channel [Real]
  //   Zeta_r         - Damping ratio for yaw channel [Real]
  // Begin default data definition:
  //  Aero coefficients
  drag_per_velsq = 0.00021;
  // Time constant for roll channel; natural frequency of stabilized airframe
  tau_p = 0.02;
  freq0_q = 8.0;
  freq0_r = 8.0;
  // Damping ratios for pitch and yaw channels
  zeta_r = 0.5;
  zeta_q = 0.5;
  // Minimum turning radii for x-y and x-z planes
  rmin_xy = 200.0;
  rmin_xz = 200.0;
  // Aerodynamic reference angles-of-attack
  alpha_ref_dg = 15.0;
  beta_ref_dg = 15.0;
}

void airframe_response_init() {
  // Airframe response model initialization
  //
  // Inputs:
  //   Alpha_Ref_dg    - Reference alpha [deg]
  //   Beta_Ref_dg     - Reference beta [deg]
  //   Freq0_q         - Natural frequency for pitch channel [Hz]
  //   Freq0_r         - Natural frequency for yaw channel [Hz]
  //
  // Outputs:
  //   Alpha_Ref - Reference alpha [rad]
  //   Beta_Ref  - Reference beta [rad]
  //   Omega0_q  - Natural frequency for pitch channel [rad/sec]
  //   Omega0_r  - Natural frequency for yaw channel [rad/sec]
  //
  // Internal variables and constants:
  //   PI      - Ratio of a circle's circumference to it's diameter [Real]
  //   RDTODG  - Radians to degrees conversion factor [deg/rad]
  //
  // Declare global common and assign variable locations:
  // Begin math model initialization:
  // Convert from input units to simulation units
  alpha_ref = alpha_ref_dg/rdtodg;
  beta_ref = beta_ref_dg/rdtodg;
  omega0_q = 2.0*pi*freq0_q;
  omega0_r = 2.0*pi*freq0_r;
}

void airframe_response() {
  // This model determines the rate of change of the angular and translational
  // velocities due to aerodynamic forces resulting from acceleration commands.
  // Transfer functions approximate the behavior of a stable
  // aerodynamics/autopilot combination. This version models the aerodynamic
  // drag as a function of total velocity.
  //
  // Inputs:
  //   Alpha_Ref       - Reference alpha [rad]
  //   Beta_Ref        - Reference beta [rad]
  //   Drag_Per_VelSq  - Base axial drag coefficient [Real]
  //   Omega0_q        - Natural frequency for pitch channel [rad/sec]
  //   Omega0_r        - Natural frequency for yaw channel [rad/sec]
  //   P_b             - Missile inertial angular velocity [rad/sec]
  //   Q_b
  //   R_b
  //   P_b_Cmd         - Commanded inertial angular velocity [rad/sec]
  //   Q_b_Cmd
  //   R_b_Cmd
  //   Q0_b            - ICS to BCS quaternion [Real]
  //   Q1_b
  //   Q2_b
  //   Q3_b
  //   Rmin_xy         - Minimum turning radius in X-Y plane (BCS) [m]
  //   Rmin_xz         - Minimum turning radius in X-Z plane (BCS) [m]
  //   Tau_p           - Time constant for roll channel [sec]
  //   Xd_bi_i         - Missile velocity in ICS [m/sec]
  //   Yd_bi_i
  //   Zd_bi_i
  //   Zeta_q          - Damping ratio for pitch channel [Real]
  //   Zeta_r          - Damping ratio for yaw channel [Real]
  //
  // Outputs:
  //   AaeroX_bi_b     - Accelerations due to aero in BCS [m/sec**2]
  //   AaeroY_bi_b
  //   AaeroZ_bi_b
  //   Alpha           - Angle of attack [rad]
  //   Beta            - Sideslip angle [rad]
  //   Pd_b            - Angular accelerations WRT ICS in BCS [rad/sec**2]
  //   Qd_b
  //   Rd_b
  //   Tibij           - ICS to BCS transformation matrix [Real]
  //   Vmag            - Total missile velocity WRT earth [m/sec]
  //
  // Internal variables and constants:
  //   Acc_Alpha       - Pitch acceleration due to alpha [m/sec**2]
  //   Acc_Beta        - Yaw acceleration due to beta [m/sec**2]
  //   Acc_Per_Alpha   - Pitch acceleration per rad of alpha [m/sec**2/rad]
  //   Acc_Per_Beta    - Yaw acceleration per rad of beta [m/sec**2/rad]
  //   Alpha_Cmd       - Commanded alpha [rad]
  //   Adrag           - X-axis (BCS) deceleration due to drag [m/sec**2]
  //   Aqij            - State advance matrix for Qd_b [Real]
  //   Arij            - State advance matrix for Rd_b [Real]
  //   Beta_Cmd        - Commanded beta [rad]
  //   Beta_prime      - Out-of-plane sideslip angle [rad]
  //   Bqi             - Input matrix for Qd_b [Real]
  //   Bri             - Input matrix for Rd_b [Real]
  //   CAlpha          - Cosine of alpha [Real]
  //   CBetap          - Cosine of beta [Real]
  //   SMALL           - Arbitrarily small number [Real]
  //   Tstbij          - Stability axis to BCS transformation matrix [Real]
  //   VelSq           - The square of Vmag [m**2/sec**2]
  //   Xd_bi_b         - Missile velocity WRT Earth in BCS [m/sec]
  //   Yd_bi_b
  //   Zd_bi_b
  //   Y_Acc_Max       - Yaw acceleration limit [m/sec**2]
  //   Z_Acc_Max       - Pitch acceleration limit [m/sec**2]
  REAL acc_alpha,acc_beta,acc_per_alpha,acc_per_beta,alpha_cmd,adrag,
    beta_cmd,beta_prime,calpha,cbetap,velsq,xd_bi_b,
    yd_bi_b,zd_bi_b,y_acc_max,z_acc_max,vmag,salpha,sbetap,
    tstb11,tstb12,tstb13,tstb21,tstb22,tstb23,tstb31,tstb32,tstb33,
    y_acc_cmd,br2,ar11,ar21,ar22,z_acc_cmd,
    bq2,aq11,aq21,aq22;
  // Begin math model:
  // Evaluate ICS to BCS transformation matrix
  tib11 = q0_b*q0_b + q1_b*q1_b - q2_b*q2_b - q3_b*q3_b;
  tib12 = 2.0*(q1_b*q2_b+q0_b*q3_b);
  tib13 = 2.0*(q1_b*q3_b-q0_b*q2_b);
  tib21 = 2.0*(q1_b*q2_b-q0_b*q3_b);
  tib22 = q0_b*q0_b + q2_b*q2_b - q1_b*q1_b - q3_b*q3_b;
  tib23 = 2.0*(q2_b*q3_b+q0_b*q1_b);
  tib31 = 2.0*(q1_b*q3_b+q0_b*q2_b);
  tib32 = 2.0*(q2_b*q3_b-q0_b*q1_b);
  tib33 = q0_b*q0_b + q3_b*q3_b - q1_b*q1_b - q2_b*q2_b;
  // Missile velocity WRT ICS origin in BCS
  xd_bi_b = xd_bi_i*tib11 + yd_bi_i*tib12 + zd_bi_i*tib13;
  yd_bi_b = xd_bi_i*tib21 + yd_bi_i*tib22 + zd_bi_i*tib23;
  zd_bi_b = xd_bi_i*tib31 + yd_bi_i*tib32 + zd_bi_i*tib33;
  velsq = xd_bi_i*xd_bi_i + yd_bi_i*yd_bi_i + zd_bi_i*zd_bi_i;
  vmag = sqrt(velsq);
  // Angle of attack and sideslip angle
  alpha = atan2(zd_bi_b,xd_bi_b);
  beta = atan2(yd_bi_b,xd_bi_b);
  beta_prime = atan2(yd_bi_b,sqrt(xd_bi_b*xd_bi_b+zd_bi_b*zd_bi_b));
  // Aero angle trig functions
  calpha = cos(alpha);
  cbetap = cos(beta_prime);
  salpha = sin(alpha);
  sbetap = sin(beta_prime);
  // Stability axis to body axis transformation matrix
  tstb11 = calpha*cbetap;
  tstb12 = -calpha*sbetap;
  tstb13 = -salpha;
  tstb21 = sbetap;
  tstb22 = cbetap;
  tstb23 = 0.0;
  tstb31 = salpha*cbetap;
  tstb32 = -salpha*sbetap;
  tstb33 = calpha;
  // Translational accelerations due to aero in stability axis
  adrag = -velsq*drag_per_velsq;
  z_acc_max = velsq/rmin_xz;
  acc_per_alpha = -MAX(small,z_acc_max/alpha_ref);
  acc_alpha = acc_per_alpha*alpha;
  y_acc_max = velsq/rmin_xy;
  acc_per_beta = -MAX(small,y_acc_max/beta_ref);
  acc_beta = acc_per_beta*beta;
  // Translational acceleration due to aero in BCS
  aaerox_bi_b = tstb11*adrag + tstb12*acc_beta + tstb13*acc_alpha;
  aaeroy_bi_b = tstb21*adrag + tstb22*acc_beta + tstb23*acc_alpha;
  aaeroz_bi_b = tstb31*adrag + tstb32*acc_beta + tstb33*acc_alpha;
  // Roll response to roll command
  pd_b = (p_b_cmd-p_b)/tau_p;
  // Yaw response to a yaw command
  y_acc_cmd = r_b_cmd*vmag;
  beta_cmd = limit(y_acc_cmd,-y_acc_max,y_acc_max)/acc_per_beta;
  br2 = -omega0_r*omega0_r;
  ar11 = -vmag/(beta_ref*rmin_xy);
  ar21 = ar11*(ar11-2.0*zeta_r*omega0_r) - br2;
  ar22 = ar11 - 2.0*zeta_r*omega0_r;
  rd_b = ar21*beta + ar22*r_b + br2*beta_cmd;
  // Pitch response to a pitch command
  z_acc_cmd = -q_b_cmd*vmag;
  alpha_cmd = limit(z_acc_cmd,-z_acc_max,z_acc_max)/acc_per_alpha;
  bq2 = omega0_q*omega0_q;
  aq11 = -vmag/(alpha_ref*rmin_xz);
  aq21 = -(aq11*(aq11-2.0*zeta_q*omega0_q)+bq2);
  aq22 = aq11 - 2.0*zeta_q*omega0_q;
  qd_b = aq21*alpha + aq22*q_b + bq2*alpha_cmd;
}
