C
        Subroutine Airframe_response_Data
C
C       Airframe response model default data
C
C       Inputs:
C           None.
C
C       Outputs:
C           Alpha_Ref               - Reference alpha [rad]
C           Drag_Per_VelSq  - Axial drag coefficient {Real]
C           Omega0_q                - Natural frequency for pitch channel [rad/sec]
C           Rmin_xz                 - Minimum turning radius in X-Z plane (BCS) [m]
C           Zeta q                  - Damping ratio for pitch channel [Real]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations:
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(102), Alpha_Ref)
      Equivalence (Real_Array(103), Drag_Per_VelSq)
      Equivalence (Real_Array(104), Rmin_xz)
      Equivalence (Real_Array(105), Omega0_q)
      Equivalence (Real_Array(106), Zeta_q)
C
C       Begin default data definition:
C
C       Aero coefficients
C
      Drag_Per_VelSq = 0.00021e0
C
C       Natural frequency and damping ratio of stabilized airframe
C
      Omega0_q = 30.0e0
      Zeta_q   = 0.5e0
C
C       Minimum turning radii for x-z plane
C
      Rmin_xz = 200.0e0
C
C       Aerodynamic reference angle-of-attack
C
      Alpha_Ref = 0.2618e0
C
      Return
      End

      Subroutine Airframe_Response_Init
C
C       Airframe response model initialization
C
C       Inputs:
C           None.
C
C       Outputs:
C           None.
C
C       Internal variables and constants:
C           None.
C
C       Begin math model initialization:
C
C       No initialization required.
C
      Return
      End

      Subroutine Airframe_Response
C
C       Airframe response model
C
C       This model determines the rate of change of the angular and translational
C       velocities due to aerodynamic forces resulting from acceleration commands.
C       Transfer functions approximate the behavior of a stable
C       aerodynamics/autopilot combination. This version models the aerodynamic
C       drag as a function of total velocity.
C
C       Inputs:
C       Alpha_Ref       - Reference alpha [rad]
C       Drag_Per_VelSq  - Base axial drag coefficient [Real]
C       Omega0_q        - Natural frequency for pitch channel [rad/sec]
C       Q_b             - Attitude rate of missile [rad/sec]
C       Q_b_Cmd         - Commanded attitude rate of missile [rad/sec]
C       Rmin_xz         - Minimum turning radius in X-Z plane (BCS) [m]
C       Threat_b        - Attitude of missile [rad]
C       Xd_bi_i         - X Missile velocity in ICS [m/sec]
C       Zd_bi_i         - Y Missile velocity in ICS [m/sec]
C       Zeta_q          - Damping ratio for pitch channel [Real]
C
C     Outputs:
C       AaeroX_bi_b     - Accelerations due to aero in BCS [m/sec**2]
C       AaeroZ_bi_b
C       Acc_Alpha       - Pitch acceleration due to alpha [m/sec**2]
C       Acc_Per_Alpha   - Pitch acceleration per rad of alpha [m/sec**2/rad]
C       Adrag           - X-axis (BCS) deceleration due to drag [m/sec**2]
C       Alpha           - Angle of attack [rad]
C       Alpha_Cmd       - Commanded alpha [rad]
C       Aqij            - State advance matrix for Qd_b [Real]
C       Bqi             - Input matrix for Qd_b [Real]
C       Beta            - Sideslip angle [rad]
C       Qd_b
C       VelSq           - The square of Vmag [m**2/sec**2]
C       Vmag            - Total missile velocity WRT earth [m/sec]
C       Xd_bi_b         - X Missile velocity WRT Earth in BCS [m/sec]
C       Z_Acc_Max       - Pitch acceleration limit [m/sec**2]
C       Zd_bi_b         - Y Missile velocity WRT Earth in BCS [m/sec]
C
C       Internal variables and constants:
C               None.
C
C       Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(102), Alpha_Ref)
      Equivalence (Real_Array(103), Drag_Per_VelSq)
      Equivalence (Real_Array(105), Omega0_q)
      Equivalence (Real_Array(104), Rmin_xz)
      Equivalence (Real_Array(106), Zeta_q)
      Equivalence  (Real_Array(100), AaeroX_bi_b)
      Equivalence  (Real_Array(101), AaeroZ_bi_b)
      Equivalence  (Real_Array(107), Acc_Alpha)
      Equivalence  (Real_Array(108), Acc_Per_Alpha)
      Equivalence  (Real_Array(109), Adrag)
      Equivalence  (Real_Array(110), Alpha)
      Equivalence  (Real_Array(111), Alpha_Cmd)
      Equivalence  (Real_Array(112), Aq11)

      Equivalence  (Real_Array(113), Aq21)
      Equivalence  (Real_Array(114), Aq22)
      Equivalence  (Real_Array(115), Bq2)
      Equivalence  (Real_Array(116), VelSq)
      Equivalence  (Real_Array(117), Vmag)
      Equivalence  (Real_Array(118), Xd_bi_b)
      Equivalence  (Real_Array(119), Z_Acc_Max)
      Equivalence  (Real_Array(120), Zd_bi_b)
      Equivalence  (Real_Array(300), Q_b_Cmd)
      Equivalence  (Real_Array(501), Xd_bi_i)
      Equivalence  (Real_Array(504), Zd_bi_i)
      Equivalence  (Real_Array(506), Theta_b)
      Equivalence  (Real_Array(507), Q_b)
      Equivalence  (Real_Array(508), Qd_b)

C
C       Begin math model:
C
C       Missile velocity in missile axis
C
      Xd_bi_b = Xd_bi_i*Cos(Theta_b) - Zd_bi_i*Sin(Theta_b)
      Zd_bi_b = Xd_bi_i*Sin(Theta_b) + Zd_bi_i*Cos(Theta_b)
C
C       Missile velocity WRT ICS origin in BCS
C
      VelSq = Xd_bi_i*Xd_bi_i + Zd_bi_i*Zd_bi_i
      Vmag = Sqrt(VelSq)
C
C       Angle of attack
C
      Alpha = ArcTan(Zd_bi_b, Xd_bi_b)
C
C       Translational accelerations due to aero in stability axis
C
      Adrag           = VelSq*Drag_Per_VelSq
      Z_Acc_Max       = VelSq/Rmin_xz
      Acc_Per_Alpha   =  - Amax1(1.0e-6, Z_Acc_Max/Alpha_Ref)
      Acc_Alpha       = Acc_Per_Alpha*Alpha
C
C       Translational accelerations due to aero in BCS
C
      AaeroX_bi_b = Adrag*Cos(Alpha) - Acc_Alpha*sin(Alpha)
      Aaeroz_bi_b = Adrag*Sin(Alpha) + Acc_Alpha*Cos(Alpha)
C
C       Pitch response to a pitch command
C
      Alpha_Cmd       = Limit(-Q_b_Cmd*Vmag, -Z_Acc_Max, Z_Acc_Max)/
     &                      Acc_Per_Alpha
      Bq2             = Omega0_q*Omega0_q
      Aq11            = -Vmag/(Alpha_Ref*Rmin_xz)
      Aq21            = -(Aq11*(Aq11 -  2.0*Zeta_q*Omega0_q) + Bq2)
      Aq22            = Aq11 - 2.0*Zeta_q*Omega0_q
      Qd_b            = Aq21*Alpha + Aq22*Q_b + Bq2*Alpha_Cmd
C
      Return
      End
