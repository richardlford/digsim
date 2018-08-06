C
      Subroutine Flight_Computer_Data
C
C     This subroutine sets the default data for the flight computer.
C
C     Inputs:
C       None.
C
C     Outputs:
C       Del_Cmd_Per_P_Cmd   - Effective fin deflection command per commanded angular velocity [rad/rad/sec]
C       Del_Cmd_per_Q_Cmd
C       Del_Cmd_Per_R_Cmd
C       Psi_b_Est_dg        - Estimated missile Euler angles [deg]
C       Theta_b_Est_dg
C       Phi_b_Est_dg
C       Q_b_Cmd_Bias        - Bias on pitch rate command [rad/sec]
C       Roll_Guidance_Gain  - Guidance gain for roll channel [Real]
C       Pitch_Guidance_Gain - Yaw guidance gain for terminal homing [Real]
C       Yaw_Guidance_Gain   - Pitch guidance gain for terminal homing [Real]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Equivalence (Real_Array (312), Pitch_Guidance_Gain)
      Equivalence (Real_Array (313), Roll_Guidance_Gain)
      Equivalence (Real_Array (314), Yaw_Guidance_Gain)
      Equivalence (Real_Array (315), Psi_b_Est_dg)
      Equivalence (Real_Array (316), Theta_b_Est_dg)
      Equivalence (Real_Array (317), Phi_b_Est_dg)
      Equivalence (Real_Array (318), Psi_b_Est)
      Equivalence (Real_Array (319), Theta_b_Est)
      Equivalence (Real_Array (320), Phi_b_Est)
      Equivalence (Real_Array (321), Q_b_CMD_Bias)
      Equivalence (Real_Array (322), Del_Cmd_Per_P_Cmd)
      Equivalence (Real_Array (323), Del_Cmd_Per_q_Cmd)
      Equivalence (Real_Array (324), Del_Cmd_Per_R_Cmd)
C
C     Begin default data definiton:
C
C     Guidance gains
C
      Roll_Guidance_Gain  = -20.0e0
      Pitch_Guidance_Gain =   4.0e0
      Yaw_Guidance_Gain   =   4.0e0
C
C     Autopilot gains
C
      Del_Cmd_Per_P_Cmd = 0.01e0
      Del_Cmd_Per_Q_Cmd = 1.0e0
      Del_Cmd_Per_R_Cmd = 1.0e0
C
C     Initial Euler angle estimates
C
      Psi_b_Est_dg   = 0.0e0
      Theta_b_Est_dg = 0.0e0
      Phi_b_Est_dg   = 0.0e0
C
C     Pitch rate command bias (gravity compensation)
C
      Q_b_Cmd_Bias = 0.0988e0
C
      Return
      End

      Subroutine Flight_Computer_Init
C
C     This subroutine sets the initial conditions for the flight computer.
C
C     Inputs:
C       Psi_b_Est_dg   - Estimated missile Euler angles [degs]
C       Theta_b_Est_dg
C       Phi_b_Est_dg
C
C     Outputs:
C       Q0_b_Est - Estimated ICS to BCS quaternion [Real]
C       Q1_b_Est
C       Q2_b_Est
C       Q3_b_Est
C
C     Internal variables and constants:
C       CPsiO2   - Cosines of half Euler angles [Real]
C       CThetaO2
C       CPhiO2
C       RDTODG   - Radians to degrees conversion factor [deg/rad]
C       SPsiO2   - Sines of half Euler angles [Real]
C       SThetaO2
C       SPhiO2
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Equivalence (Real_Array ( 51), RDTODG)
      Equivalence (Real_Array (304), Q0_b_Est)
      Equivalence (Real_Array (305), Q1_b_Est)
      Equivalence (Real_Array (306), Q2_b_Est)
      Equivalence (Real_Array (307), Q3_b_Est)
      Equivalence (Real_Array (315), Psi_b_Est_dg)
      Equivalence (Real_Array (316), Theta_b_Est_dg)
      Equivalence (Real_Array (317), Phi_b_Est_dg)
C
C     Begin flight computer initialization:
C
C     Define flight computer states
C
      Call Define_Real_State (304, 308)
      Call Define_Real_State (305, 309)
      Call Define_Real_State (306, 310)
      Call Define_Real_State (307, 311)
C
C     Initial flight computer states
C
C     Trig functions of half Euler angles
C
      CPsiO2   = Cos (0.5e0*Psi_b_Est_dg/RDTODG)
      CThetaO2 = Cos (0.5e0*Theta_b_Est_dg/RDTODG)
      CPhiO2   = Cos (0.5e0*Phi_b_Est_dg/RDTODG)
      SPsiO2   = Sin (0.5e0*Psi_b_Est_dg/RDTODG)
      SThetaO2 = Sin (0.5e0*Theta_b_Est_dg/RDTODG)
      SPhiO2   = Sin (0.5e0*Phi_b_Est_dg/RDTODG)
C
C     Initial quaternion values
C
      Q0_b_Est = CPsiO2*CThetaO2*CPhiO2 + SPsiO2*SThetaO2*SPhiO2
      Q1_b_Est = CPsiO2*CThetaO2*SPhiO2 + SPsiO2*SThetaO2*CPhiO2
      Q2_b_Est = CPsiO2*SThetaO2*CPhiO2 + SPsiO2*CThetaO2*SPhiO2
      Q3_b_Est = SPsiO2*CThetaO2*CPhiO2 + CPsiO2*SThetaO2*SPhiO2
C
      Return
      End

      Subroutine Flight_Computer
C
C     This subroutine provides a continuous time emulation of the MLRS/TGSM
C     flight computer processing.
C
C     Inputs:
C       Del_Cmd_Per_P_Cmd   - Effective fin deflection command per commanded
C       Del_Cmd_Per_Q_Cmd     angular velocity [rad/rad/sec]
C       Del_Cmd_Per_R_Cmd
C       P_g_Meas            - Measured angular velocity [rad/sec]
C       Q_g_Meas
C       R_g_Meas
C       Q_si_b_Meas         - LOS rates expressed in the BCS [rad/sec]
C       R_si_b_Meas
C       Q_b_Cmd_Bias        - Bias on pitch rate command [rad/sec]
C       Roll_Guidance_Gain  - Guidance gain for roll channel [Real]
C       Pitch_Guidance_Gain - Yaw Guidance gain for terminal homing [Real]
C       Yaw_Guidance_Gain   - Pitch guidance gain for terminal homing [Real]
C
C     Outputs:
C       Fin_1_Cmd   - Fin position commands [rad]
C       Fin_2_Cmd
C       Fin_3_Cmd
C       Fin_4_Cmd
C       P_b_Cmd     - Commanded inertial angular velocity [rad/sec]
C       Q_b_Cmd
C       R_b_Cmd
C       Psi_b_Est   - Estimated missile Euler angles [rad]
C       Theta_b_Est
C       Phi_b_Est
C       Q0_b_Est    - Estimated ICS to BCS quaternion [Real]
C       Q1_b_Est
C       Q2_b_Est
C       Q3_b_Est
C       Q0d_b_Est   - Estimated ICS to BCS quaternion derivatives [Real]
C       Q1d_b_Est
C       Q2d_b_Est
C       Q3d_b_Est
C
C     Internal variables and constants:
C       Del_P_Cmd    - Commanded equivalent fin deflections [rad]
C       Del_Q_Cmd
C       Del_R_Cmd
C       Qmag_Est     - Magnitude of estimated quaternion [Real]
C       STheta_b_Est - Sine of estimated pitch Euler angle
C       Tibij_Est    - Estimated elements of the ICS to BCS transformation matrix [Real]
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array (200), P_g_Meas)
      Equivalence (Real_Array (201), Q_g_Meas)
      Equivalence (Real_Array (202), R_g_Meas)
      Equivalence (Real_Array (600), Q_si_b_Meas)
      Equivalence (Real_Array (601), R_si_b_Meas)
      Equivalence (Real_Array (300), Fin_1_Cmd)
      Equivalence (Real_Array (301), Fin_2_Cmd)
      Equivalence (Real_Array (302), Fin_3_Cmd)
      Equivalence (Real_Array (303), Fin_4_Cmd)
      Equivalence (Real_Array (304), Q0_b_Est)
      Equivalence (Real_Array (305), Q1_b_Est)
      Equivalence (Real_Array (306), Q2_b_Est)
      Equivalence (Real_Array (307), Q3_b_Est)
      Equivalence (Real_Array (308), Q0d_b_Est)
      Equivalence (Real_Array (309), Q1d_b_Est)
      Equivalence (Real_Array (310), Q2d_b_Est)
      Equivalence (Real_Array (311), Q3d_b_Est)
      Equivalence (Real_Array (312), Pitch_Guidance_Gain)
      Equivalence (Real_Array (313), Roll_Guidance_Gain)
      Equivalence (Real_Array (314), Yaw_Guidance_Gain)
      Equivalence (Real_Array (318), Psi_b_Est)
      Equivalence (Real_Array (319), Theta_b_Est)
      Equivalence (Real_Array (320), Phi_b_Est)
      Equivalence (Real_Array (321), Q_b_Cmd_Bias)
      Equivalence (Real_Array (322), Del_Cmd_Per_P_Cmd)
      Equivalence (Real_Array (323), Del_Cmd_Per_Q_Cmd)
      Equivalence (Real_Array (324), Del_Cmd_Per_R_Cmd)
      Equivalence (Real_Array (325), P_b_Cmd)
      Equivalence (Real_Array (326), Q_b_Cmd)
      Equivalence (Real_Array (327), R_b_Cmd)
C
C     Begin flight computer emulation:
C
C     Navigation section
C
C     Evaluate the ICS to BCS transformation matrix
C
      Tib11_est = Q0_b_est*Q0_b_est + Q1_b_est*Q1_b_est -
     &            Q2_b_est*Q2_b_est - Q3_b_est*Q3_b_est
      Tib12_est = 2.0e0*(Q1_b_est*Q2_b_est + Q0_b_est*Q3_b_est)
      Tib13_est = 2.0e0*(Q1_b_est*Q3_b_est - Q0_b_est*Q2_b_est)
      Tib21_est = 2.0e0*(Q1_b_est*Q2_b_est - Q0_b_est*Q3_b_est)
      Tib22_est = Q0_b_est*Q0_b_est + Q2_b_est*Q2_b_est -
     &            Q1_b_est*Q1_b_est - Q3_b_est*Q3_b_est
      Tib23_est = 2.0e0*(Q2_b_est*Q3_b_est + Q0_b_est*Q1_b_est)
      Tib31_est = 2.0e0*(Q1_b_est*Q3_b_est + Q0_b_est*Q2_b_est)
      Tib32_est = 2.0e0*(Q2_b_est*Q3_b_est - Q0_b_est*Q1_b_est)
      Tib33_est = Q0_b_est*Q0_b_est + Q3_b_est*Q3_b_est -
     &            Q1_b_est*Q1_b_est - Q2_b_est*Q2_b_est
C
C     Evaluate Euler roll angle
C
      STheta_b_est = Limit (-Tib13_est, -1.0e0, 1.0e0)
      Psi_b_est    = ArcTan (Tib12_est, Tib11_est)
      Theta_b_est  = Asin (STheta_b_est)
      Phi_b_est    = ArcTan (Tib23_est, Tib33_est)
C
C     Quaternion constraint equation
C
      Qmag_Est = Sqrt (Q0_b_Est*Q0_b_Est +
     &                 Q1_b_Est*Q1_b_Est +
     &                 Q2_b_Est*Q2_b_Est +
     &                 Q3_b_Est*Q3_b_Est)
      Q0_b_Est = Q0_b_Est/Qmag_Est
      Q1_b_Est = Q1_b_Est/Qmag_Est
      Q2_b_Est = Q2_b_Est/Qmag_Est
      Q3_b_Est = Q3_b_Est/Qmag_Est
C
C     Quaternion derivative
C
      Q0d_b_Est = -0.5e0*(Q1_b_Est*P_g_Meas +
     &                    Q2_b_Est*Q_g_Meas +
     &                    Q3_b_Est*R_g_Meas)
      Q1d_b_Est =  0.5e0*(Q0_b_Est*P_g_Meas +
     &                    Q2_b_Est*R_g_Meas -
     &                    Q3_b_Est*Q_g_Meas)
      Q2d_b_Est =  0.5e0*(Q0_b_Est*Q_g_Meas +
     &                    Q3_b_Est*P_g_Meas -
     &                    Q1_b_Est*R_g_Meas)
      Q3d_b_Est =  0.5e0*(Q0_b_Est*R_g_Meas +
     &                    Q1_b_Est*Q_g_Meas -
     &                    Q2_b_Est*P_g_Meas)
C
C     Maintain zero roll angle
C
      P_b_Cmd = Roll_Guidance_Gain*Phi_b_Est
C
C     Compute yaw & pitch rate command via proportional navigation
C
      Q_b_Cmd = Pitch_Guidance_Gain*Q_si_b_Meas +
     &          Q_b_Cmd_Bias*Cos(Theta_b_Est)
      R_b_Cmd = Yaw_Guidance_Gain*R_si_b_Meas
C
C     Autopilot
C
      Del_P_Cmd = Del_Cmd_Per_P_Cmd*P_b_Cmd
      Del_Q_Cmd = Del_Cmd_Per_Q_Cmd*Q_b_Cmd
      Del_R_Cmd = Del_Cmd_Per_R_Cmd*R_b_Cmd
C
C     Fin mixing
C
      Fin_1_Cmd = -Del_Q_Cmd - Del_P_Cmd
      Fin_2_Cmd = -Del_R_Cmd - Del_P_Cmd
      Fin_3_Cmd =  Del_Q_Cmd - Del_P_Cmd
      Fin_4_Cmd =  Del_R_Cmd - Del_P_Cmd
C
      Return
      End
