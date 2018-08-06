C
        Subroutine Kinematics_Data
C       This subroutine sets the default data for the kinematics routine.
C
C       Inputs:
C           None.
C
C       Outputs:
C           Theta_b_IC_dg - Initial attitude of missile [deg]
C           Velocity - Velocity of missile [m/sec]
C           X_bi_i_IC - Initial X position of BCS w.r t ICS, expressed in the
C                                   ICS [m]
C           Z_bi_i_IC - Initial Z position of BCS w.r t ICS, expressed in the
C                                   ICS [m]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array(508), Theta_b_IC_dg)
        Equivalence (Real_Array(510), Velocity)
        Equivalence (Real_Array(506), X_bi_i_IC)
        Equivalence (Real_Array(507), Z_bi_i_IC)
C
C       Begin default data definitions:
C
        Theta_b_IC_dg =    0.0e0
        Velocity      =  100.0e0
        X_bi_i_IC     = -500.0e0
        Z_bi_i_IC     = -100.0e0
C
        Return
        End
C
        Subroutine Kinematics_Init
C
C       This subroutine initializes the variables for the kinematics routine
C
C       Inputs:
C           RDTODG - Radians to degrees conversion factor [deg/rad]
C           Theta_b_IC_dg - Initial attitude of missile [deg]
C           X_bi_i_IC - Initial X position of BCS w.r t ICS, expressed in the
C                                   ICS [m]
C           Z_pi_i_IC - Initial Z position of BCS w.r t ICS, expressed in the
C                                   ICS [m]
C
C       Outputs:
C           Theta_b - Attitude of missile [rad]
C           X_bi_i - X position of BCS w.r.t. ICS, expressed in the ICS [m]
C           Z_bi_i - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array( 50), RDTODG)
        Equivalence (Real_Array(504), Theta_b)
        Equivalence (Real_Array(500), X_bi_i)
        Equivalence (Real_Array(501), Z_bi_i)
        Equivalence (Real_Array(508), Theta_b_IC_dg)
        Equivalence (Real_Array(506), X_bi_i_IC)
        Equivalence (Real_Array(507), Z_bi_i_IC)
C
C       Begin math model initialization:
C
C       Define the states
C
        Call Define_Real_State(500, 502)
        Call Define_Real_State(501, 503)
        Call Define_Real_State(504, 505)
C
C       Set the initial conditions
C
        X_bi_i = X_bi_i_IC
        Z_bi_i = Z_bi_i_IC
        Theta_b = Theta_b_IC_dg/RDTODG
C
        Return
        End

        Subroutine Kinematics
C
C       This subroutine determines the kinematics state derivatives
C
C       Inputs:
C               Theta_b - Attitude of missile [rad]
C               Velocity - Velocity of missile [m/sec]
C
C       Outputs:
C               Xd_bi_i - X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
C               Zd_bi_i - Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
C
C       Internal variables and constants:
C
C       Declare global common and assign variable locations
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array(510), Velocity)
        Equivalence (Real_Array(502), Xd_bi_i)
        Equivalence (Real_Array(503), Zd_bi_i)
        Equivalence (Real_Array(504), Theta_b)
C
C       Begin math model:
C
C       Calculate velocities (attitude rate comes directly from airframe response)
        Xd_bi_i = Velocity*Cos(Theta_b)
        Zd_bi_i = -Velocity*Sin(Theta_b)
C
        Return
        End
