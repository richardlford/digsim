#include "airframe.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/limit.h"
#include <math.h>

#define aaerox_bi_b    real_array[100].r
#define aaeroz_bi_b    real_array[101].r
#define alpha_ref      real_array[102].r
#define drag_per_velsq real_array[103].r
#define rmin_xz        real_array[104].r
#define omega0_q       real_array[105].r
#define zeta_q         real_array[106].r
#define acc_alpha      real_array[107].r
#define acc_per_alpha  real_array[108].r
#define adrag          real_array[109].r
#define alpha          real_array[110].r
#define alpha_cmd      real_array[111].r
#define aq11           real_array[112].r
#define aq21           real_array[113].r
#define aq22           real_array[114].r
#define bq2            real_array[115].r
#define velsq          real_array[116].r
#define vmag           real_array[117].r
#define xd_bi_b        real_array[118].r
#define z_acc_max      real_array[119].r
#define zd_bi_b        real_array[120].r
#define q_b_cmd        real_array[300].r
#define xd_bi_i        real_array[501].r
#define zd_bi_i        real_array[504].r
#define theta_b        real_array[506].r
#define q_b            real_array[507].r
#define qd_b           real_array[508].r

void airframe_response_data() {
  // Airframe response model default data
  // Inputs:
  //     None.
  // Outputs:
  //     Alpha_Ref               - Reference alpha [rad]
  //     Drag_Per_VelSq  - Axial drag coefficient {Real]
  //     Omega0_q                - Natural frequency for pitch channel [rad/sec]
  //     Rmin_xz                 - Minimum turning radius in X-Z plane (BCS) [m]
  //     Zeta q                  - Damping ratio for pitch channel [Real]
  // Aero coefficients
  drag_per_velsq = 0.00021;
  // Natural frequency and damping ratio of stabilized airframe
  omega0_q = 30.0;
  zeta_q = 0.5;
  // Minimum turning radii for x-z plane
  rmin_xz = 200.0;
  // Aerodynamic reference angle-of-attack
  alpha_ref = 0.2618;
}

void airframe_response_init() {
  // Airframe response model initialization
  // Inputs:
  //     None.
  // Outputs:
  //     None.
  // Internal variables and constants:
  //     None.
  // Begin math model initialization:
  // No initialization required.
}

void airframe_response() {
  //   Airframe response model
  //
  //   This model determines the rate of change of the angular and translational
  //   velocities due to aerodynamic forces resulting from acceleration commands.
  //   Transfer functions approximate the behavior of a stable
  //   aerodynamics/autopilot combination. This version models the aerodynamic
  //   drag as a function of total velocity.
  //
  //   Inputs:
  //   Alpha_Ref       - Reference alpha [rad]
  //   Drag_Per_VelSq  - Base axial drag coefficient [Real]
  //   Omega0_q        - Natural frequency for pitch channel [rad/sec]
  //   Q_b             - Attitude rate of missile [rad/sec]
  //   Q_b_Cmd         - Commanded attitude rate of missile [rad/sec]
  //   Rmin_xz         - Minimum turning radius in X-Z plane (BCS) [m]
  //   Threat_b        - Attitude of missile [rad]
  //   Xd_bi_i         - X Missile velocity in ICS [m/sec]
  //   Zd_bi_i         - Y Missile velocity in ICS [m/sec]
  //   Zeta_q          - Damping ratio for pitch channel [Real]
  //
  // Outputs:
  //   AaeroX_bi_b     - Accelerations due to aero in BCS [m/sec**2]
  //   AaeroZ_bi_b
  //   Acc_Alpha       - Pitch acceleration due to alpha [m/sec**2]
  //   Acc_Per_Alpha   - Pitch acceleration per rad of alpha [m/sec**2/rad]
  //   Adrag           - X-axis (BCS) deceleration due to drag [m/sec**2]
  //   Alpha           - Angle of attack [rad]
  //   Alpha_Cmd       - Commanded alpha [rad]
  //   Aqij            - State advance matrix for Qd_b [Real]
  //   Bqi             - Input matrix for Qd_b [Real]
  //   Beta            - Sideslip angle [rad]
  //   Qd_b
  //   VelSq           - The square of Vmag [m**2/sec**2]
  //   Vmag            - Total missile velocity WRT earth [m/sec]
  //   Xd_bi_b         - X Missile velocity WRT Earth in BCS [m/sec]
  //   Z_Acc_Max       - Pitch acceleration limit [m/sec**2]
  //   Zd_bi_b         - Y Missile velocity WRT Earth in BCS [m/sec]
  //
  //   Internal variables and constants:
  //           None.
  //
  //   Declare global common and assign variable locations
  // Begin math model:
  // Missile velocity in missile axis
  xd_bi_b = xd_bi_i*cos(theta_b) - zd_bi_i*sin(theta_b);
  zd_bi_b = xd_bi_i*sin(theta_b) + zd_bi_i*cos(theta_b);
  // Missile velocity WRT ICS origin in BCS
  velsq = xd_bi_i*xd_bi_i + zd_bi_i*zd_bi_i;
  vmag = sqrt(velsq);
  // Angle of attack
  alpha = atan2(zd_bi_b,xd_bi_b);
  // Translational accelerations due to aero in stability axis
  adrag = velsq*drag_per_velsq;
  z_acc_max = velsq/rmin_xz;
  acc_per_alpha = -MAX(0.000001,z_acc_max/alpha_ref);
  acc_alpha = acc_per_alpha*alpha;
  // Translational accelerations due to aero in BCS
  aaerox_bi_b = adrag*cos(alpha) - acc_alpha*sin(alpha);
  aaeroz_bi_b = adrag*sin(alpha) + acc_alpha*cos(alpha);
  // Pitch response to a pitch command
  alpha_cmd = limit(-q_b_cmd*vmag,-z_acc_max,z_acc_max)/acc_per_alpha;
  bq2 = omega0_q*omega0_q;
  aq11 = -vmag/(alpha_ref*rmin_xz);
  aq21 = -(aq11*(aq11-2.0*zeta_q*omega0_q)+bq2);
  aq22 = aq11 - 2.0*zeta_q*omega0_q;
  qd_b = aq21*alpha + aq22*q_b + bq2*alpha_cmd;
}
