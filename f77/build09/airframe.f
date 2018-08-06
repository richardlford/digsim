C
      Subroutine Airframe_Response_Data
C
C     Airframe response model default data
C
C     Inputs:
C       None.
C
C     Outputs:
C       Alpha_Ref_dg   - Reference alpha (deg)
C       Beta_Ref_dg    - Reference beta (deg)
C       Drag_Per_VelSq - Base axial drag coefficient [Real]
C       Freq0_q        - Natural frequency for pitch channel [Hz]
C       Freq0_r        - Natural frequency for yaw channel [Hz]
C       Rmin_xy        - Minimum turning radius in X-Y plane (BCS) [m]
C       Rmin_xz        - Minimum turning radius in X-Z plane (BCS) [m]
C       Tau_p          - Time constant for roll channel [sec]
C       Zeta_q         - Damping ratio for pitch channel [Real]
C       Zeta_r         - Damping ratio for yaw channel [Real]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations:
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence  (Real_Array(115), Alpha_Ref_dg)
      Equivalence  (Real_Array(116), Beta_Ref_dg)
      Equivalence  (Real_Array(107), Drag_Per_VelSq)
      Equivalence  (Real_Array(117), Freq0_q)
      Equivalence  (Real_Array(118), Freq0_r)
      Equivalence  (Real_Array(108), Rmin_xy)
      Equivalence  (Real_Array(109), Rmin_xz)
      Equivalence  (Real_Array(110), Tau_p)
      Equivalence  (Real_Array(113), Zeta_q)
      Equivalence  (Real_Array(114), Zeta_r)
C
C     Begin default data definition:
C
C     Aero coefficients
C
      Drag_Per_VelSq = 0.00021e0
C
C     Time constant for roll channel; natural frequency of stabilized airframe
C
      Tau_p   = 0.02e0
      Freq0_q = 8.0e0
      Freq0_r = 8.0e0
C
C     Damping ratios for pitch and yaw channels
C
      Zeta_r = 0.5e0
      Zeta_q = 0.5e0
C
C     Minimum turning radii for x-y and x-z planes
C
      Rmin_xy = 200.0
      Rmin_xz = 200.0
C
C     Aerodynamic reference angles-of-attack
C
      Alpha_Ref_dg = 15.0e0
      Beta_Ref_dg  = 15.0e0
C
      Return
      End

      Subroutine Airframe_Response_Init
C
C     Airframe response model initialization
C
C     Inputs:
C       Alpha_Ref_dg    - Reference alpha [deg]
C       Beta_Ref_dg     - Reference beta [deg]
C       Freq0_q         - Natural frequency for pitch channel [Hz]
C       Freq0_r         - Natural frequency for yaw channel [Hz]
C
C     Outputs:
C       Alpha_Ref - Reference alpha [rad]
C       Beta_Ref  - Reference beta [rad]
C       Omega0_q  - Natural frequency for pitch channel [rad/sec]
C       Omega0_r  - Natural frequency for yaw channel [rad/sec]
C
C     Internal variables and constants:
C       PI      - Ratio of a circle's circumference to it's diameter [Real]
C       RDTODG  - Radians to degrees conversion factor [deg/rad]
C
C     Declare global common and assign variable locations:
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence  (Real_Array( 50), PI)
      Equivalence  (Real_Array( 51), RDTODG)
      Equivalence  (Real_Array(104), Alpha_Ref)
      Equivalence  (Real_Array(106), Beta_Ref)
      Equivalence  (Real_Array(111), Omega0_q)
      Equivalence  (Real_Array(112), Omega0_r)
      Equivalence  (Real_Array(115), Alpha_Ref_dg)
      Equivalence  (Real_Array(116), Beta_Ref_dg)
      Equivalence  (Real_Array(117), Freq0_q)
      Equivalence  (Real_Array(118), Freq0_r)
C
C     Begin math model initialization:
C
C     Convert from input units to simulation units
C
      Alpha_Ref = Alpha_Ref_dg/RDTODG
      Beta_Ref  = Beta_Ref_dg/RDTODG
      Omega0_q  = 2.0e0*PI*Freq0_q
      Omega0_r  = 2.0e0*PI*Freq0_r
C
      Return
      End

      Subroutine Airframe_Response
C
C     This model determines the rate of change of the angular and translational
C     velocities due to aerodynamic forces resulting from acceleration commands.
C     Transfer functions approximate the behavior of a stable
C     aerodynamics/autopilot combination. This version models the aerodynamic
C     drag as a function of total velocity.
C
C     Inputs:
C       Alpha_Ref       - Reference alpha [rad]
C       Beta_Ref        - Reference beta [rad]
C       Drag_Per_VelSq  - Base axial drag coefficient [Real]
C       Omega0_q        - Natural frequency for pitch channel [rad/sec]
C       Omega0_r        - Natural frequency for yaw channel [rad/sec]
C       P_b             - Missile inertial angular velocity [rad/sec]
C       Q_b
C       R_b
C       P_b_Cmd         - Commanded inertial angular velocity [rad/sec]
C       Q_b_Cmd
C       R_b_Cmd
C       Q0_b            - ICS to BCS quaternion [Real]
C       Q1_b
C       Q2_b
C       Q3_b
C       Rmin_xy         - Minimum turning radius in X-Y plane (BCS) [m]
C       Rmin_xz         - Minimum turning radius in X-Z plane (BCS) [m]
C       Tau_p           - Time constant for roll channel [sec]
C       Xd_bi_i         - Missile velocity in ICS [m/sec]
C       Yd_bi_i
C       Zd_bi_i
C       Zeta_q          - Damping ratio for pitch channel [Real]
C       Zeta_r          - Damping ratio for yaw channel [Real]
C
C     Outputs:
C       AaeroX_bi_b     - Accelerations due to aero in BCS [m/sec**2]
C       AaeroY_bi_b
C       AaeroZ_bi_b
C       Alpha           - Angle of attack [rad]
C       Beta            - Sideslip angle [rad]
C       Pd_b            - Angular accelerations WRT ICS in BCS [rad/sec**2]
C       Qd_b
C       Rd_b
C       Tibij           - ICS to BCS transformation matrix [Real]
C       Vmag            - Total missile velocity WRT earth [m/sec]
C
C     Internal variables and constants:
C       Acc_Alpha       - Pitch acceleration due to alpha [m/sec**2]
C       Acc_Beta        - Yaw acceleration due to beta [m/sec**2]
C       Acc_Per_Alpha   - Pitch acceleration per rad of alpha [m/sec**2/rad]
C       Acc_Per_Beta    - Yaw acceleration per rad of beta [m/sec**2/rad]
C       Alpha_Cmd       - Commanded alpha [rad]
C       Adrag           - X-axis (BCS) deceleration due to drag [m/sec**2]
C       Aqij            - State advance matrix for Qd_b [Real]
C       Arij            - State advance matrix for Rd_b [Real]
C       Beta_Cmd        - Commanded beta [rad]
C       Beta_prime      - Out-of-plane sideslip angle [rad]
C       Bqi             - Input matrix for Qd_b [Real]
C       Bri             - Input matrix for Rd_b [Real]
C       CAlpha          - Cosine of alpha [Real]
C       CBetap          - Cosine of beta [Real]
C       SMALL           - Arbitrarily small number [Real]
C       Tstbij          - Stability axis to BCS transformation matrix [Real]
C       VelSq           - The square of Vmag [m**2/sec**2]
C       Xd_bi_b         - Missile velocity WRT Earth in BCS [m/sec]
C       Yd_bi_b
C       Zd_bi_b
C       Y_Acc_Max       - Yaw acceleration limit [m/sec**2]
C       Z_Acc_Max       - Pitch acceleration limit [m/sec**2]
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence  (Real_Array( 51), RDTODG)
      Equivalence  (Real_Array( 52), SMALL)
      Equivalence  (Real_Array(100), AaeroX_bi_b)
      Equivalence  (Real_Array(101), AaeroY_bi_b)
      Equivalence  (Real_Array(102), AaeroZ_bi_b)
      Equivalence  (Real_Array(103), Alpha)
      Equivalence  (Real_Array(104), Alpha_Ref)
      Equivalence  (Real_Array(105), Beta)
      Equivalence  (Real_Array(106), Beta_Ref)
      Equivalence  (Real_Array(107), Drag_Per_VelSq)
      Equivalence  (Real_Array(108), Rmin_xy)
      Equivalence  (Real_Array(109), Rmin_xz)
      Equivalence  (Real_Array(110), Tau_p)
      Equivalence  (Real_Array(111), Omega0_q)
      Equivalence  (Real_Array(112), Omega0_r)
      Equivalence  (Real_Array(113), Zeta_q)
      Equivalence  (Real_Array(114), Zeta_r)
      Equivalence  (Real_Array(501), Xd_bi_i)
      Equivalence  (Real_Array(504), Yd_bi_i)
      Equivalence  (Real_Array(507), Zd_bi_i)
      Equivalence  (Real_Array(509), P_b)
      Equivalence  (Real_Array(511), Q_b)
      Equivalence  (Real_Array(513), R_b)
      Equivalence  (Real_Array(510), Pd_b)
      Equivalence  (Real_Array(512), Qd_b)
      Equivalence  (Real_Array(514), Rd_b)
      Equivalence  (Real_Array(518), Q0_b)
      Equivalence  (Real_Array(520), Q1_b)
      Equivalence  (Real_Array(522), Q2_b)
      Equivalence  (Real_Array(524), Q3_b)
      Equivalence  (Real_Array(526), Tib11)
      Equivalence  (Real_Array(527), Tib12)
      Equivalence  (Real_Array(528), Tib13)
      Equivalence  (Real_Array(529), Tib21)
      Equivalence  (Real_Array(530), Tib22)
      Equivalence  (Real_Array(531), Tib23)
      Equivalence  (Real_Array(532), Tib31)
      Equivalence  (Real_Array(533), Tib32)
      Equivalence  (Real_Array(534), Tib33)
      Equivalence  (Real_Array(300), P_b_Cmd)
      Equivalence  (Real_Array(301), Q_b_Cmd)
      Equivalence  (Real_Array(302), R_b_Cmd)
C
C     Begin math model:
C
C     Evaluate ICS to BCS transformation matrix
C
      Tib11 = Q0_b*Q0_b + Q1_b*Q1_b - Q2_b*Q2_b - Q3_b*Q3_b
      Tib12 = 2.0*(Q1_b*Q2_b + Q0_b*Q3_b)
      Tib13 = 2.0*(Q1_b*Q3_b - Q0_b*Q2_b)
      Tib21 = 2.0*(Q1_b*Q2_b - Q0_b*Q3_b)
      Tib22 = Q0_b*Q0_b + Q2_b*Q2_b - Q1_b*Q1_b - Q3_b*Q3_b
      Tib23 = 2.0*(Q2_b*Q3_b + Q0_b*Q1_b)
      Tib31 = 2.0*(Q1_b*Q3_b + Q0_b*Q2_b)
      Tib32 = 2.0*(Q2_b*Q3_b - Q0_b*Q1_b)
      Tib33 = Q0_b*Q0_b + Q3_b*Q3_b - Q1_b*Q1_b - Q2_b*Q2_b
C
C     Missile velocity WRT ICS origin in BCS
C
      Xd_bi_b = Xd_bi_i*Tib11 + Yd_bi_i*Tib12 + Zd_bi_i*Tib13
      Yd_bi_b = Xd_bi_i*Tib21 + Yd_bi_i*Tib22 + Zd_bi_i*Tib23
      Zd_bi_b = Xd_bi_i*Tib31 + Yd_bi_i*Tib32 + Zd_bi_i*Tib33
      VelSq   = Xd_bi_i*Xd_bi_i +
     &          Yd_bi_i*Yd_bi_i +
     &          Zd_bi_i*Zd_bi_i
      Vmag    = Sqrt (VelSq)
C
C     Angle of attack and sideslip angle
C
      Alpha           = ArcTan(Zd_bi_b, Xd_bi_b)
      Beta            = ArcTan(Yd_bi_b, Xd_bi_b)
      Beta_prime      = ArcTan(Yd_bi_b,
     &                        Sqrt(Xd_bi_b*Xd_bi_b + Zd_bi_b*Zd_bi_b))
C
C     Aero angle trig functions
C
      CAlpha = Cos(Alpha)
      CBetap = Cos(Beta_prime)
      SAlpha = Sin(Alpha)
      SBetap = Sin(Beta_prime)
C
C     Stability axis to body axis transformation matrix
C
      Tstb11 =  CAlpha*CBetap
      Tstb12 = -CAlpha*SBetap
      Tstb13 = -SAlpha
      Tstb21 =  SBetap
      Tstb22 =  CBetap
      Tstb23 =  0.0e0
      Tstb31 =  SAlpha*CBetap
      Tstb32 = -SAlpha*SBetap
      Tstb33 =  CAlpha
C
C     Translational accelerations due to aero in stability axis
C
      Adrag           = -VelSq*Drag_Per_VelSq
      Z_Acc_Max       =  VelSq/Rmin_xz
      Acc_Per_Alpha   = -Amax1 (SMALL, Z_Acc_Max/Alpha_Ref)
      Acc_Alpha       =  Acc_Per_Alpha*Alpha
      Y_Acc_Max       =  VelSq/Rmin_xy
      Acc_Per_Beta    = -Amax1 (SMALL, Y_Acc_Max/Beta_Ref)
      Acc_Beta        =  Acc_Per_Beta*Beta
C
C     Translational acceleration due to aero in BCS
C
      AaeroX_bi_b = Tstb11*Adrag + Tstb12*Acc_Beta + Tstb13*Acc_Alpha
      AaeroY_bi_b = Tstb21*Adrag + Tstb22*Acc_Beta + Tstb23*Acc_Alpha
      AaeroZ_bi_b = Tstb31*Adrag + Tstb32*Acc_Beta + Tstb33*Acc_Alpha
C
C     Roll response to roll command
C
      Pd_b = (P_b_Cmd - P_b)/Tau_p
C
C     Yaw response to a yaw command
C
      Y_Acc_Cmd =  R_b_Cmd*Vmag
      Beta_Cmd  =  Limit(Y_Acc_Cmd, -Y_Acc_Max, Y_Acc_Max)/Acc_Per_Beta
      Br2       = -Omega0_r*Omega0_r
      Ar11      = -Vmag/(Beta_Ref*Rmin_xy)
      Ar21      =  Ar11*(Ar11 - 2.0*Zeta_r*Omega0_r) - Br2
      Ar22      =  Ar11 - 2.0*Zeta_r*Omega0_r
      Rd_b      =  Ar21*Beta + Ar22*R_b + Br2*Beta_Cmd
C
C     Pitch response to a pitch command
C
      Z_Acc_Cmd = -Q_b_Cmd*Vmag
      Alpha_Cmd =  Limit(Z_Acc_Cmd, -Z_Acc_Max, Z_Acc_Max)/Acc_Per_Alpha
      Bq2       =  Omega0_q*Omega0_q
      Aq11      = -Vmag/(Alpha_Ref*Rmin_xz)
      Aq21      = -(Aq11*(Aq11 - 2.0*Zeta_q*Omega0_q) + Bq2)
      Aq22      =  Aq11 - 2.0*Zeta_q*Omega0_q
      Qd_b      =  Aq21*Alpha + Aq22*Q_b + Bq2*Alpha_Cmd

      Return
      End
