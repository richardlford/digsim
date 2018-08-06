C
        Subroutine Kinematics_Data
C
C       This subroutine sets the Default data for the kinematics routine.
C
C       Inputs:
C           None.
C
C       Outputs:
C           Gravity                 - Acceleration due to gravity [m/sec**2]
C           Theta_b_IC_dg   - Initial attitude of missile [deg]
C           Q_b_IC_dg               - Initial attitude rate of missile [deg/sec]
C           X_bi_i_IC               - Initial X position of BCS w.r.t. ICS, expressed in the
C                                             ICS [m]
C           Xd_bi_i_IC              - Initial X velocity of BCS w.r.t. ICS, expressed
C                                             ICS [m/sec]
C           Z_bi_i_IC               - Initial Z position of BCS w.r.t. ICS, expressed
C                                             ICS [m]
C           Zd_bi_i_IC              - Initial Z velocity of BCS w.r.t. ICS, expressed
C                                             ICS [m/sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(509), X_bi_i_IC)
      Equivalence (Real_Array(510), Xd_bi_i_IC)
      Equivalence (Real_Array(511), Z_bi_i_IC)
      Equivalence (Real_Array(512), Zd_bi_i_IC)
      Equivalence (Real_Array(513), Theta_b_IC_dg)
      Equivalence (Real_Array(514), Q_b_IC_dg)
      Equivalence (Real_Array(515), Gravity)
C
C       Begin default data definitions:
C
      Theta_b_IC_dg =    0.0e0
      Q_b_IC_dg     =    0.0e0
      X_bi_i_IC     = -500.0e0
      Z_bi_i_IC     = -100.0e0
      Xd_bi_i_IC    =  100.0e0
      Zd_bi_i_IC    =    0.0e0
      Gravity       =    9.88e0
C
      Return
      End

      Subroutine Kinematics_Init
C
C       This subroutine initializes the variables for the kinematics routine.
C
C       Inputs:
C           RDTODG        - Radians to degrees conversion factor [deg/rad]
C           Theta_b_IC_dg - Initial attitude of missile [deg]
C           Q_b_IC_dg     - Initial attitude rate of missile [deg/sec]
C           X_bi_i_IC     - Initial X position of BCS w.r.t. ICS, expressed in the
C                           ICS [m]
C           Xd_bi_i_IC    - Initial X velocity of BCS w.r.t. ICS, expressed
C                           ICS [m/sec]
C           Z_bi_i_IC     - Initial Z position of BCS w.r.t. ICS, expressed
C                           ICS [m]
C           Zd_bi_i_IC    - Initial Z velocity of BCS w.r.t. ICS, expressed
C                           ICS [m/sec]
C
C       Outputs:
C           Theta_b       - Attitude of missile [rad]
C           Q_b           - Attitude rate of missile [rad/sec]
C           X_bi_i        - X position of BCS w.r.t. ICS, expressed in the ICS [m]
C           Xd_bi_i       - X velocity of BCS w.r.t. ICS, expressed ICS [m/sec]
C           Z_bi_i        - Z position of BCS w.r.t. ICS, expressed ICS [m]
C           Zd_bi_i       - Z velocity of BCS w.r.t. ICS, expressed ICS [m/sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array( 50), RDTODG)
      Equivalence (Real_Array(500), X_bi_i)
      Equivalence (Real_Array(501), Xd_bi_i)
      Equivalence (Real_Array(503), Z_bi_i)
      Equivalence (Real_Array(504), Zd_bi_i)
      Equivalence (Real_Array(506), Theta_b)
      Equivalence (Real_Array(507), Q_b)
      Equivalence (Real_Array(509), X_bi_i_IC)
      Equivalence (Real_Array(510), Xd_bi_i_IC)
      Equivalence (Real_Array(511), Z_bi_i_IC)
      Equivalence (Real_Array(512), Zd_bi_i_IC)
      Equivalence (Real_Array(513), Theta_b_IC_dg)
      Equivalence (Real_Array(514), Q_b_IC_dg)

C       Begin math model initialization
C
C       Define the states
C
      Call Define_Real_State(500, 501)
      Call Define_Real_State(501, 502)
      Call Define_Real_State(503, 504)
      Call Define_Real_State(504, 505)
      Call Define_Real_State(506, 507)
      Call Define_Real_State(507, 508)
C
C       Set the initial conditions
C
      X_bi_i          = X_bi_i_IC
      Z_bi_i          = Z_bi_i_IC
      Xd_bi_i         = Xd_bi_i_IC
      Zd_bi_i         = Zd_bi_i_IC
      Theta_b         = Theta_b_IC_dg/RDTODG
      Q_b             = Q_b_IC_dg/RDTODG
C
      Return
      End

      Subroutine Kinematics
C
C       This subroutine determined the kinematic state derivatives.
C
C       Inputs:
C           AaeroX_bi_b - Accelerations due to aero in BCS [mXsec**2)
C           AaeroZ_hi_b
C           Gravity - Acceleration due to gravity [m/sec**2]
C           Theta_b - Attitude of missile [rad]
C
C       Outputs:
C           Xdd_bi_i - X acceleration of BCS w.r.t. ICS, expressed in the ICS [m/sec]
C           Zdd_bi_i - Z acceleration of BCS w.r.t. ICS expressed in the ICS [m/sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(100), AaeroX_bi_b)
      Equivalence (Real_Array(101), AaeroZ_bi_b)
      Equivalence (Real_Array(502), Xdd_bi_i)
      Equivalence (Real_Array(505), Zdd_bi_i)
      Equivalence (Real_Array(506), Theta_b)
      Equivalence (Real_Array(515), Gravity)
C
C       Begin math model:
C
C       Calculate accelerations (attitude rate derivative comes from aero)
C
      Xdd_bi_i =  AaeroX_bi_b*Cos(Theta_b) + AaeroZ_bi_b*Sin(Theta_b)
      Zdd_bi_i = -AaeroX_bi_b*Sin(Theta_b) + AaeroZ_bi_b*Cos(Theta_b)+
     &               Gravity
C
      Return
      End
