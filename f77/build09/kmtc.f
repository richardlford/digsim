C
        Subroutine Kinematics_Data
C
C       This subroutine sets the Default data for the kinematics routine.
C
C       Inputs:
C           None.
C
C     Outputs:
C       Acc_Gravity   - Acceleration due to gravity [m/sec**2]
C       Psi_b_IC_dg   - Initial Euler angles [deg]
C       Theta_b_IC_dg
C       Phi_b_IC_dg
C       P_b_IC_dg     - Initial missile rotation velocity [deg/sec]
C       Q_b_IC_dg
C       R_b_IC_dg
C       Xd_bi_i_IC    - Initial missile velocity WRT ICS [m/sec]
C       Yd_bi_i_IC
C       Zd_bi_i_IC
C       X_bi_i_IC     - Initial missile position [m]
C       Y_bi_i_IC
C       Z_bi_i_IC
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(536), Psi_b_IC_dg)
      Equivalence (Real_Array(537), Theta_b_IC_dg)
      Equivalence (Real_Array(538), Phi_b_IC_dg)
      Equivalence (Real_Array(539), P_b_IC_dg)
      Equivalence (Real_Array(540), Q_b_IC_dg)
      Equivalence (Real_Array(541), R_b_IC_dg)
      Equivalence (Real_Array(542), Xd_bi_i_IC)
      Equivalence (Real_Array(543), Yd_bi_i_IC)
      Equivalence (Real_Array(544), Zd_bi_i_IC)
      Equivalence (Real_Array(545), X_bi_i_IC)
      Equivalence (Real_Array(546), Y_bi_i_IC)
      Equivalence (Real_Array(547), Z_bi_i_IC)
      Equivalence (Real_Array(548), Acc_Gravity)
C
C     Begin default data definitions:
C
C     Variables for determining initial attitude
C
      Psi_b_IC_dg   = 0.0e0
      Theta_b_IC_dg = 0.0e0
      Phi_b_IC_dg   = 0.0e0
C
C     Variables for determining initial angular velocity
C
      P_b_IC_dg = 0.0e0
      Q_b_IC_dg = 0.0e0
      R_b_IC_dg = 0.0e0
C
C     Variables for determining initial translational velocity of the BCS w.r.t.
C     the ICS
C
      Xd_bi_i_IC = 100.0e0
      Yd_bi_i_IC =   0.0e0
      Zd_bi_i_IC =   0.0e0
C
C     Variables for determining initial translational position of the BCS w.r.t.
C     the ICS
C
      X_bi_i_IC =    0.0e0
      Y_bi_i_IC =    0.0e0
      Z_bi_i_IC = -100.0e0
C
C     Acceleration due to gravity
C
      Acc_Gravity = 9.88e0
C
      Return
      End

      Subroutine Kinematics_Init
C
C     Missile kinematics model initialization; initializes quaternions from
C     initial missile Euler angles.
C
C     Inputs:
C       Psi_b_IC_dg   - Initial Euler angles [deg]
C       Theta_b_IC_dg
C       Phi_b_IC_dg
C       P_b_IC_dg     - Initial missile rotational velocity [deg/sec]
C       Q_b_IC_dg
C       R_b_IC_dg
C       Xd_bi_i_IC    - Initial missile velocity WRT ICS [m/sec]
C       Yd_bi_i_IC
C       Zd_bi_i_IC
C       X_bi_i_IC     - Initial missile position [m]
C       Y_bi_i_IC
C       Z_bi_i_IC
C
C     Outputs:
C       P_b     - Missile inertial angular velocity [rad/sec]
C       Q_b
C       R_b
C       Q0_b    - ICS to BCS quaternion [Real]
C       Q1_b
C       Q2_b
C       Q3_b
C       X_bi_i  - Missile terminal position WRT ICS [m]
C       Y_bi_i
C       Z_bi_i
C       Xd_bi_i - Missile velocity in ICS [m/sec]
C       Yd_bi_i
C       Zd_bi_i
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
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array( 51), RDTODG)
      Equivalence (Real_Array(500), X_bi_i)
      Equivalence (Real_Array(501), Xd_bi_i)
      Equivalence (Real_Array(503), Y_bi_i)
      Equivalence (Real_Array(504), Yd_bi_i)
      Equivalence (Real_Array(506), Z_bi_i)
      Equivalence (Real_Array(507), Zd_bi_i)
      Equivalence (Real_Array(509), P_b)
      Equivalence (Real_Array(511), Q_b)
      Equivalence (Real_Array(513), R_b)
      Equivalence (Real_Array(515), Psi_b)
      Equivalence (Real_Array(516), Theta_b)
      Equivalence (Real_Array(517), Phi_b)
      Equivalence (Real_Array(518), Q0_b)
      Equivalence (Real_Array(520), Q1_b)
      Equivalence (Real_Array(522), Q2_b)
      Equivalence (Real_Array(524), Q3_b)
      Equivalence (Real_Array(536), Psi_b_IC_dg)
      Equivalence (Real_Array(537), Theta_b_IC_dg)
      Equivalence (Real_Array(538), Phi_b_IC_dg)
      Equivalence (Real_Array(539), P_b_IC_dg)
      Equivalence (Real_Array(540), Q_b_IC_dg)
      Equivalence (Real_Array(541), R_b_IC_dg)
      Equivalence (Real_Array(542), Xd_bi_i_IC)
      Equivalence (Real_Array(543), Yd_bi_i_IC)
      Equivalence (Real_Array(544), Zd_bi_i_IC)
      Equivalence (Real_Array(545), X_bi_i_IC)
      Equivalence (Real_Array(546), Y_bi_i_IC)
      Equivalence (Real_Array(547), Z_bi_i_IC)
C
C     Assign state and state derivative pointers
C
      Call Define_Real_State(500, 501)
      Call Define_Real_State(501, 502)
      Call Define_Real_State(503, 504)
      Call Define_Real_State(504, 505)
      Call Define_Real_State(506, 507)
      Call Define_Real_State(507, 508)
      Call Define_Real_State(509, 510)
      Call Define_Real_State(511, 512)
      Call Define_Real_State(513, 514)
      Call Define_Real_State(518, 519)
      Call Define_Real_State(520, 521)
      Call Define_Real_State(522, 523)
      Call Define_Real_State(524, 525)
C
C     Begin math model initialization
C
C     Initial Euler angles: (BCS) = [ROLL][PITCH][YAW](ICS)
C
      Psi_b   = Psi_b_IC_dg/RDTODG
      Theta_b = Theta_b_IC_dg/RDTODG
      Phi_b   = Phi_b_IC_dg/RDTODG
C
C     Initial angular velocity of BCS w.r.t. ICS, expressed in BCS
C
      P_b = P_b_IC_dg/RDTODG
      Q_b = Q_b_IC_dg/RDTODG
      R_b = R_b_IC_dg/RDTODG
C
C     Initial translational position of the BCS w.r.t. the ICS
C
      X_bi_i = X_bi_i_IC
      Y_bi_i = Y_bi_i_IC
      Z_bi_i = Z_bi_i_IC
C
C     Initial translational velocity of the BCS w.r.t. to the ICS
C
      Xd_bi_i = Xd_bi_i_IC
      Yd_bi_i = Yd_bi_i_IC
      Zd_bi_i = Zd_bi_i_IC
C
C     Trig functions of half Euler angles
C
      CPsiO2   = Cos (0.5e0*Psi_b)
      CThetaO2 = Cos (0.5e0*Theta_b)
      CPhiO2   = Cos (0.5e0*Phi_b)
      SPsiO2   = Sin (0.5e0*Psi_b)
      SThetaO2 = Sin (0.5e0*Theta_b)
      SPhiO2   = Sin (0.5e0*Phi_b)
C
C     Initial quaternion values
C
      Q0_b = CPsiO2*CThetaO2*CPhiO2 + SPsiO2*SThetaO2*SPhiO2
      Q1_b = CPsiO2*CThetaO2*SPhiO2 - SPsiO2*SThetaO2*CPhiO2
      Q2_b = CPsiO2*SThetaO2*CPhiO2 + SPsiO2*CThetaO2*SPhiO2
      Q3_b = SPsiO2*CThetaO2*CPhiO2 - CPsiO2*SThetaO2*SPhiO2
C
      Return
      End

      Subroutine Kinematics
C
C     This module evaluates the translational and rotational derivatives which
C     describe the missile's motion WRT an earth fixed coordinate system for
C     a non-rotating earth.
C
C     Inputs:
C       AaeroX_bi_b - Accelerations due to aero in BCS [m/sec**2]
C       AaeroY_bi_b
C       AaeroZ_bi_b
C       Acc_Gravity - Acceleration due to gravity [m/sec**2]
C       P_b         - Missile inertial angular velocity [rad/sec]
C       Q_b
C       R_b
C       Q0_b        - ICS to BCS quaternion [Real]
C       Q1_b
C       Q2_b
C       Q3_b
C       Tibij       - ICS to BCS transformation matrix [Real]
C
C     Outputs:
C       Q0_b     - ICS to BCS quaternion [Real]
C       Q1_b
C       Q2_b
C       Q3_b
C       Qd0_b    - ICS to BCS quaternion derivatives [Real]
C       Qd1_b
C       Qd2_b
C       Qd3_b
C       Psi_b    - Missile Euler angles [rad]
C       Theta_b
C       Phi_b
C       Xdd_bi_i - Missile acceleration WRT ICS in ICS [m/sec**2]
C       Ydd_bi_i
C       Zdd_bi_i
C
C     Internal variables and constants:
C       Qmag     - Magnitude of ICS to BCS quaternion [Real]
C       RDTODG   - Radians to degrees conversion factor [deg/rad]
C       STheta_b - Sine of pitch Euler angle [Real]
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array( 51), RDTODG)
      Equivalence (Real_Array(100), AaeroX_bi_b)
      Equivalence (Real_Array(101), AaeroY_bi_b)
      Equivalence (Real_Array(102), AaeroZ_bi_b)
      Equivalence (Real_Array(502), Xdd_bi_i)
      Equivalence (Real_Array(505), Ydd_bi_i)
      Equivalence (Real_Array(508), Zdd_bi_i)
      Equivalence (Real_Array(509), P_b)
      Equivalence (Real_Array(511), Q_b)
      Equivalence (Real_Array(513), R_b)
      Equivalence (Real_Array(515), Psi_b)
      Equivalence (Real_Array(516), Theta_b)
      Equivalence (Real_Array(517), Phi_b)
      Equivalence (Real_Array(518), Q0_b)
      Equivalence (Real_Array(519), Q0d_b)
      Equivalence (Real_Array(520), Q1_b)
      Equivalence (Real_Array(521), Q1d_b)
      Equivalence (Real_Array(522), Q2_b)
      Equivalence (Real_Array(523), Q2d_b)
      Equivalence (Real_Array(524), Q3_b)
      Equivalence (Real_Array(525), Q3d_b)
      Equivalence (Real_Array(526), Tib11)
      Equivalence (Real_Array(527), Tib12)
      Equivalence (Real_Array(528), Tib13)
      Equivalence (Real_Array(529), Tib21)
      Equivalence (Real_Array(530), Tib22)
      Equivalence (Real_Array(531), Tib23)
      Equivalence (Real_Array(532), Tib31)
      Equivalence (Real_Array(533), Tib32)
      Equivalence (Real_Array(534), Tib33)
      Equivalence (Real_Array(548), Acc_Gravity)
C
C     Begin math model:
C
C     Evaluate Euler angles
C
      STheta_b = Limit (-Tib13, -1.0e0, 1.0e0)
      Theta_b  = Asin (STheta_b)
      Psi_b    = ArcTan (Tib12, Tib11)
      Phi_b    = ArcTan (Tib23, Tib33)
C
C     Missile velocity rate in ICS WRT ICS
C
      Xdd_bi_i = Tib11*AaeroX_bi_b +
     &           Tib21*AaeroY_bi_b +
     &           Tib31*AaeroZ_bi_b
      Ydd_bi_i = Tib12*AaeroX_bi_b +
     &           Tib22*AaeroY_bi_b +
     &           Tib32*AaeroZ_bi_b
      Zdd_bi_i = Tib13*AaeroX_bi_b +
     &           Tib23*AaeroY_bi_b +
     &           Tib33*AaeroZ_bi_b +
     &           Acc_Gravity
C
C     Quaternion constraint equation
C
      Qmag = Sqrt (Q0_b*Q0_b + Q1_b*Q1_b + Q2_b*Q2_b + Q3_b*Q3_b)
      Q0_b = Q0_b/Qmag
      Q1_b = Q1_b/Qmag
      Q2_b = Q2_b/Qmag
      Q3_b = Q3_b/Qmag
C
C     Quaternion derivative
C
      Q0d_b = -0.5e0*(Q1_b*P_b + Q2_b*Q_b + Q3_b*R_b)
      Q1d_b =  0.5e0*(Q0_b*P_b + Q2_b*R_b - Q3_b*Q_b)
      Q2d_b =  0.5e0*(Q0_b*Q_b + Q3_b*P_b - Q1_b*R_b)
      Q3d_b =  0.5e0*(Q0_b*R_b + Q1_b*Q_b - Q2_b*P_b)
C
      Return
      End
