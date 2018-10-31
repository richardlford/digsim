#include "fcomp.h"
#include "../driver/global.h"
#include "../driver/defstate.h"
#include "../driver/limit.h"
#include <math.h>

#define rdtodg              real_array[51].r
#define p_g_meas            real_array[200].r
#define q_g_meas            real_array[201].r
#define r_g_meas            real_array[202].r
#define fin_1_cmd           real_array[300].r
#define fin_2_cmd           real_array[301].r
#define fin_3_cmd           real_array[302].r
#define fin_4_cmd           real_array[303].r
#define q0_b_est            real_array[304].r
#define q1_b_est            real_array[305].r
#define q2_b_est            real_array[306].r
#define q3_b_est            real_array[307].r
#define q0d_b_est           real_array[308].r
#define q1d_b_est           real_array[309].r
#define q2d_b_est           real_array[310].r
#define q3d_b_est           real_array[311].r
#define pitch_guidance_gain real_array[312].r
#define roll_guidance_gain  real_array[313].r
#define yaw_guidance_gain   real_array[314].r
#define psi_b_est_dg        real_array[315].r
#define theta_b_est_dg      real_array[316].r
#define phi_b_est_dg        real_array[317].r
#define psi_b_est           real_array[318].r
#define theta_b_est         real_array[319].r
#define phi_b_est           real_array[320].r
#define q_b_cmd_bias        real_array[321].r
#define del_cmd_per_p_cmd   real_array[322].r
#define del_cmd_per_q_cmd   real_array[323].r
#define del_cmd_per_r_cmd   real_array[324].r
#define p_b_cmd             real_array[325].r
#define q_b_cmd             real_array[326].r
#define r_b_cmd             real_array[327].r
#define q_si_b_meas         real_array[600].r
#define r_si_b_meas         real_array[601].r

void flight_computer_data() {
  // This subroutine sets the default data for the flight computer.
  //
  // Inputs:
  //   None.
  //
  // Outputs:
  //   Del_Cmd_Per_P_Cmd   - Effective fin deflection command per commanded
  //   Del_Cmd_per_Q_Cmd      angular velocity [rad/rad/sec]
  //   Del_Cmd_Per_R_Cmd
  //   Psi_b_Est_dg        - Estimated missile Euler angles [deg]
  //   Theta_b_Est_dg
  //   Phi_b_Est_dg
  //   Q_b_Cmd_Bias        - Bias on pitch rate command [rad/sec]
  //   Roll_Guidance_Gain  - Guidance gain for roll channel [Real]
  //   Pitch_Guidance_Gain - Yaw guidance gain for terminal homing [Real]
  //   Yaw_Guidance_Gain   - Pitch guidance gain for terminal homing [Real]
  // Begin default data definiton:
  // Guidance gains
  roll_guidance_gain = -20.0;
  pitch_guidance_gain = 4.0;
  yaw_guidance_gain = 4.0;
  // Autopilot gains
  del_cmd_per_p_cmd = 0.01;
  del_cmd_per_q_cmd = 1.0;
  del_cmd_per_r_cmd = 1.0;
  // Initial Euler angle estimates
  psi_b_est_dg = 0.0;
  theta_b_est_dg = 0.0;
  phi_b_est_dg = 0.0;
  // Pitch rate command bias (gravity compensation)
  q_b_cmd_bias = 0.0988;
}

void flight_computer_init() {
  // This subroutine sets the initial conditions for the flight computer.
  //
  // Inputs:
  //   Psi_b_Est_dg   - Estimated missile Euler angles [degs]
  //   Theta_b_Est_dg
  //   Phi_b_Est_dg
  //
  // Outputs:
  //   Q0_b_Est - Estimated ICS to BCS quaternion [Real]
  //   Q1_b_Est
  //   Q2_b_Est
  //   Q3_b_Est
  //
  // Internal variables and constants:
  //   CPsiO2   - Cosines of half Euler angles [Real]
  //   CThetaO2
  //   CPhiO2
  //   RDTODG   - Radians to degrees conversion factor [deg/rad]
  //   SPsiO2   - Sines of half Euler angles [Real]
  //   SThetaO2
  //   SPhiO2
  REAL cpsio2,cthetao2,cphio2,spsio2,sthetao2,sphio2;
  // Begin flight computer initialization:
  // Define flight computer states
  define_real_state(304,308);
  define_real_state(305,309);
  define_real_state(306,310);
  define_real_state(307,311);
  // Initial flight computer states
  // Trig functions of half Euler angles
  cpsio2 = cos(0.5*psi_b_est_dg/rdtodg);
  cthetao2 = cos(0.5*theta_b_est_dg/rdtodg);
  cphio2 = cos(0.5*phi_b_est_dg/rdtodg);
  spsio2 = sin(0.5*psi_b_est_dg/rdtodg);
  sthetao2 = sin(0.5*theta_b_est_dg/rdtodg);
  sphio2 = sin(0.5*phi_b_est_dg/rdtodg);
  // Initial quaternion values
  q0_b_est = cpsio2*cthetao2*cphio2 + spsio2*sthetao2*sphio2;
  q1_b_est = cpsio2*cthetao2*sphio2 + spsio2*sthetao2*cphio2;
  q2_b_est = cpsio2*sthetao2*cphio2 + spsio2*cthetao2*sphio2;
  q3_b_est = spsio2*cthetao2*cphio2 + cpsio2*sthetao2*sphio2;
}

void flight_computer() {
  // This subroutine provides a continuous time emulation of the MLRS/TGSM
  // flight computer processing.
  //
  // Inputs:
  //   Del_Cmd_Per_P_Cmd   - Effective fin deflection command per commanded
  //   Del_Cmd_Per_Q_Cmd     angular velocity [rad/rad/sec]
  //   Del_Cmd_Per_R_Cmd
  //   P_g_Meas            - Measured angular velocity [rad/sec]
  //   Q_g_Meas
  //   R_g_Meas
  //   Q_si_b_Meas         - LOS rates expressed in the BCS [rad/sec]
  //   R_si_b_Meas
  //   Q_b_Cmd_Bias        - Bias on pitch rate command [rad/sec]
  //   Roll_Guidance_Gain  - Guidance gain for roll channel [Real]
  //   Pitch_Guidance_Gain - Yaw Guidance gain for terminal homing [Real]
  //   Yaw_Guidance_Gain   - Pitch guidance gain for terminal homing [Real]
  //
  // Outputs:
  //   Fin_1_Cmd   - Fin position commands [rad]
  //   Fin_2_Cmd
  //   Fin_3_Cmd
  //   Fin_4_Cmd
  //   P_b_Cmd     - Commanded inertial angular velocity [rad/sec]
  //   Q_b_Cmd
  //   R_b_Cmd
  //   Psi_b_Est   - Estimated missile Euler angles [rad]
  //   Theta_b_Est
  //   Phi_b_Est
  //   Q0_b_Est    - Estimated ICS to BCS quaternion [Real]
  //   Q1_b_Est
  //   Q2_b_Est
  //   Q3_b_Est
  //   Q0d_b_Est   - Estimated ICS to BCS quaternion derivatives [Real]
  //   Q1d_b_Est
  //   Q2d_b_Est
  //   Q3d_b_Est
  //
  // Internal variables and constants:
  //   Del_P_Cmd    - Commanded equivalent fin deflections [rad]
  //   Del_Q_Cmd
  //   Del_R_Cmd
  //   Qmag_Est     - Magnitude of estimated quaternion [Real]
  //   STheta_b_Est - Sine of estimated pitch Euler angle
  //   Tibij_Est    - Estimated elements of the ICS to BCS transformation matrix [Real]
  REAL del_p_cmd,del_q_cmd,del_r_cmd,qmag_est,stheta_b_est,tibij_est,
    tib11_est,tib12_est,tib13_est,tib21_est,tib22_est,tib23_est,tib31_est,tib32_est,tib33_est;
  // Begin flight computer emulation:
  // Navigation section
  // Evaluate the ICS to BCS transformation matrix
  tib11_est = q0_b_est*q0_b_est + q1_b_est*q1_b_est - q2_b_est*q2_b_est - q3_b_est*q3_b_est;
  tib12_est = 2.0*(q1_b_est*q2_b_est+q0_b_est*q3_b_est);
  tib13_est = 2.0*(q1_b_est*q3_b_est-q0_b_est*q2_b_est);
  tib21_est = 2.0*(q1_b_est*q2_b_est-q0_b_est*q3_b_est);
  tib22_est = q0_b_est*q0_b_est + q2_b_est*q2_b_est - q1_b_est*q1_b_est - q3_b_est*q3_b_est;
  tib23_est = 2.0*(q2_b_est*q3_b_est+q0_b_est*q1_b_est);
  tib31_est = 2.0*(q1_b_est*q3_b_est+q0_b_est*q2_b_est);
  tib32_est = 2.0*(q2_b_est*q3_b_est-q0_b_est*q1_b_est);
  tib33_est = q0_b_est*q0_b_est + q3_b_est*q3_b_est - q1_b_est*q1_b_est - q2_b_est*q2_b_est;
  // Evaluate Euler roll angle
  stheta_b_est = limit(-tib13_est,-1.0,1.0);
  psi_b_est = atan2(tib12_est,tib11_est);
  theta_b_est = asin(stheta_b_est);
  phi_b_est = atan2(tib23_est,tib33_est);
  // Quaternion constraint equation
  qmag_est = sqrt(q0_b_est*q0_b_est + q1_b_est*q1_b_est + q2_b_est*q2_b_est + q3_b_est*q3_b_est);
  q0_b_est = q0_b_est/qmag_est;
  q1_b_est = q1_b_est/qmag_est;
  q2_b_est = q2_b_est/qmag_est;
  q3_b_est = q3_b_est/qmag_est;
  // Quaternion derivative
  q0d_b_est = -0.5*(q1_b_est*p_g_meas + q2_b_est*q_g_meas + q3_b_est*r_g_meas);
  q1d_b_est =  0.5*(q0_b_est*p_g_meas + q2_b_est*r_g_meas - q3_b_est*q_g_meas);
  q2d_b_est =  0.5*(q0_b_est*q_g_meas + q3_b_est*p_g_meas - q1_b_est*r_g_meas);
  q3d_b_est =  0.5*(q0_b_est*r_g_meas+q1_b_est*q_g_meas - q2_b_est*p_g_meas);
  // Maintain zero roll angle
  p_b_cmd = roll_guidance_gain*phi_b_est;
  // Compute yaw & pitch rate command via proportional navigation
  q_b_cmd = pitch_guidance_gain*q_si_b_meas + q_b_cmd_bias*cos(theta_b_est);
  r_b_cmd = yaw_guidance_gain*r_si_b_meas;
  // Autopilot
  del_p_cmd = del_cmd_per_p_cmd*p_b_cmd;
  del_q_cmd = del_cmd_per_q_cmd*q_b_cmd;
  del_r_cmd = del_cmd_per_r_cmd*r_b_cmd;
  // Fin mixing
  fin_1_cmd = -del_q_cmd - del_p_cmd;
  fin_2_cmd = -del_r_cmd - del_p_cmd;
  fin_3_cmd = del_q_cmd - del_p_cmd;
  fin_4_cmd = del_r_cmd - del_p_cmd;
}
