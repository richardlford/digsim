#include "aero.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include <math.h>

#define faerox_bi_b        real_array[100].r
#define faeroy_bi_b        real_array[101].r
#define faeroz_bi_b        real_array[102].r
#define maerox_bi_b        real_array[103].r
#define maeroy_bi_b        real_array[104].r
#define maeroz_bi_b        real_array[105].r
#define alpha              real_array[106].r
#define beta               real_array[107].r
#define alpha_total        real_array[108].r
#define air_density        real_array[109].r
#define ref_area           real_array[110].r
#define ref_length         real_array[111].r
#define cx_base            real_array[112].r
#define cx_per_alpha_total real_array[113].r
#define cy_per_beta        real_array[114].r
#define cz_per_alpha       real_array[115].r
#define cmy_per_alpha      real_array[116].r
#define cmz_per_beta       real_array[117].r
#define cmx_per_delp       real_array[118].r
#define cmy_per_delq       real_array[119].r
#define cmz_per_delr       real_array[120].r
#define cmp                real_array[121].r
#define cmq                real_array[122].r
#define cmr                real_array[123].r
#define xd_bi_i            real_array[501].r
#define yd_bi_i            real_array[504].r
#define zd_bi_i            real_array[507].r
#define p_b                real_array[509].r
#define q_b                real_array[511].r
#define r_b                real_array[513].r
#define q0_b               real_array[518].r
#define q1_b               real_array[520].r
#define q2_b               real_array[522].r
#define q3_b               real_array[524].r
#define tib11              real_array[526].r
#define tib12              real_array[527].r
#define tib13              real_array[528].r
#define tib21              real_array[529].r
#define tib22              real_array[530].r
#define tib23              real_array[531].r
#define tib31              real_array[532].r
#define tib32              real_array[533].r
#define tib33              real_array[534].r
#define fin_1_position     real_array[800].r
#define fin_2_position     real_array[801].r
#define fin_3_position     real_array[802].r
#define fin_4_position     real_array[803].r

void aerodynamics_data() {
  // Aerodynamics model default data
  //
  // Inputs:
  //   None.
  //
  // Outputs:
  //   Air_Density        - Local air density [Kg/m**2]
  //   Cx_Base            - Base axial drag coefficient [Real]
  //   Cx_Per_Alpha_Total - Change in axial drag coefficient per total angle of
  //                        attack [Real]
  //   Cy_Per_Beta        - Side force coefficient per sideslop angle [Real]
  //   Cy_Per_Alpha       - Lift force coefficient per angle of attack [Real]
  //   Cmy_Per_Alpha      - Pitching moment coefficient per angle of attack [Real]
  //   Cmz_Per_Beta       - Yawing moment coefficient per sideslip angle [Real]
  //   Cmx_Per_DelP       - Rolling moment coefficient per effective roll fin
  //                        deflection [Real]
  //   Cmy_Per_DelQ       - Yawing moment coefficient per effective yaw fin
  //                        deflection [Real]
  //   Cmz_Per_DelR       - Pitching moment coefficient per effective pitch fin
  //                        deflection [Real]
  //   Cmp                - Roll damping coefficient [Real]
  //   Cmq                - Pitch damping coefficient [Real]
  //   Cmr                - Yaw damping coefficient [Real]
  //   Ref_Area           - Reference area for aerodynamics calculations [m**2]
  //   Ref_Length         - Reference length for aerodynamic calculations [m]
  // Begin default data definition:
  // Local atmospheric density
  air_density = 1.0;
  // Base axial drag coefficient
  cx_base = 0.3;
  // Force coefficients per aerodynamic angles
  cx_per_alpha_total = -0.5;
  cy_per_beta = -50.0;
  cz_per_alpha = -50.0;
  // Moment coefficients per aerodynamic angles
  cmy_per_alpha = -50.0;
  cmz_per_beta = 50.0;
  // Moment coefficients per effective fin deflections
  cmx_per_delp = 10.0;
  cmy_per_delq = 40.0;
  cmz_per_delr = 40.0;
  // Damping coefficients
  cmp = -20.0;
  cmq = -200.0;
  cmr = -200.0;
  // Aerodynamic reference values
  ref_area = 0.01;
  ref_length = 1.0;
}

void aerodynamics_init() {
  // Aerodynamics model initialization
}

void aerodynamics() {
  // This model determines the aerodynamic forces and moments acting on the
  // missile.
  //
  // Inputs:
  //   Air_Density        - Local air density [Kg/m**2]
  //   Cx_Base            - Base axial drag coefficient [Real]
  //   Cx_Per_Alpha_Total - Change in axial drag coefficient per total angle of
  //                        attack [Real]
  //   Cy_Per_Beta        - Side force coefficient per sideslip angle [Real]
  //   Cy_Per_Alpha       - Lift force coefficient per angle of attack [Real]
  //   Cmy_Per_Alpha      - Pitching moment coefficient per angle of attack [Real]
  //   Cmz_Per_Beta       - Yawing moment coefficient per sideslip angle [Real]
  //   Cmx_Per_DelP       - Rolling moment coefficient per effective roll fin
  //                        deflection [Real]
  //   Cmy_Per_DelQ       - Yawing moment coefficient per effective yaw fin
  //                        deflection [Real]
  //   Cmz_Per_DelR       - Pitching moment coefficient per effective pitch fin
  //                        deflection [Real]
  //   Cmp                - Roll damping coefficient [Real]
  //   Cmq                - Pitch damping coefficient [Real]
  //   Cmr                - Yaw damping coefficient [Real]
  //   Fin_1_Position     - Fin positions [rad]
  //   Fin_2_Position
  //   Fin_3_Position
  //   Fin_4_Position
  //   P_b                - Missile inertial angular velocity [rad/sec]
  //   Q_b
  //   R_b
  //   Q0_b               - ICS to BCS quaternion [Real]
  //   Q1_b
  //   Q2_b
  //   Q3_b
  //   Ref_Area           - Reference area for aerodynamic calculations [m**2]
  //   Ref_Length         - Reference length for aerodynamic calculations [m]
  //   Xd_bi_i            - Missile velocity in ICS [m/sec]
  //   Yd_bi_i
  //   Zd_bi_i
  //
  // Outputs:
  //   Alpha              - Pitch angle of attack [rad]
  //   Alpha_Total        - Total angle of attack [rad]
  //   Beta               - Sideslip angle [rad]
  //   FaeroX_bi_b        - Forces due to aero in BCS [N]
  //   FaeroY_bi_b
  //   FaeroZ_bi_b
  //   MaeroX_bi_b        - Moments due to aero in BCS [N*m]
  //   MaeroY_bi_b
  //   MaeroZ_bi_b
  //   Tibij              - ICS to BCS transformation matrix [Real]
  //
  // Internal variables and constants:
  //   Del_Eff_P          - Effective fin deflections [rad]
  //   Del_Eff_Q
  //   Del_Eff_R
  //   LrefO2Vmag         - Reference length divided by twice the total velocity [sec]
  //   Qbar_B             - Dynamic pressure [N/m**2]
  //   QbarSref           - Product of dynamic pressure and reference area [N]
  //   QbarSrefLref       - Product of dynamic pressure, reference area, and
  //                        reference length [N*m]
  //   VelSq              - Total missile velocity, squared [m**2/s**2]
  //   Vmag               - Total missile velocity WRT earth [m/sec]
  //   Xd_bi_b            - Missile velocity WRT Earth in BCS [m/sec]
  //   Yd_bi_b
  //   Zd_bi_b
  REAL del_eff_p,del_eff_q,del_eff_r,lrefo2vmag,qbar_b,qbarsref,qbarsreflref,velsq,vmag,xd_bi_b,yd_bi_b,zd_bi_b;
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
  // Angle of attack, sideslip angle, and total angle-of-attack
  alpha = atan2(zd_bi_b,xd_bi_b);
  beta = atan2(yd_bi_b,xd_bi_b);
  alpha_total = atan2(sqrt(yd_bi_b*yd_bi_b+zd_bi_b*zd_bi_b), xd_bi_b);
  // Dynamic pressure and multipliers for forces and moments
  qbar_b = 0.5*air_density*velsq;
  qbarsref = qbar_b*ref_area;
  qbarsreflref = qbarsref*ref_length;
  lrefo2vmag = ref_length/(2.0*vmag);
  // Equivalent fin deflections
  del_eff_p = -0.25*(fin_1_position+fin_2_position+fin_3_position+fin_4_position);
  del_eff_q =  0.5*(fin_3_position-fin_1_position);
  del_eff_r =  0.5*(fin_4_position-fin_2_position);
  // Aerodynamics forces
  faerox_bi_b = (cx_base+cx_per_alpha_total*alpha_total)*qbarsref;
  faeroy_bi_b = cy_per_beta*beta*qbarsref;
  faeroz_bi_b = cz_per_alpha*alpha*qbarsref;
  // Aerodynamics moments
  maerox_bi_b = qbarsreflref*(cmx_per_delp*del_eff_p + cmp*p_b*lrefo2vmag);
  maeroy_bi_b = qbarsreflref*(cmy_per_alpha*alpha + cmy_per_delq*del_eff_q + cmq*q_b*lrefo2vmag);
  maeroz_bi_b = qbarsreflref*(cmz_per_beta*beta + cmz_per_delr*del_eff_r + cmr*r_b*lrefo2vmag);
}
