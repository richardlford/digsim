C
        Subroutine Seeker_Data
C
C       This subroutine sets the default data for the seeker model.
C
C       Inputs:
C           None.
C
C       Outputs:
C           None.
C
C       Internal variables and constants
C           None.
C
C       Begin default data definitions:
C
C       No default data required.
C
        Return
        End

        Subroutine Seeker_Init
C
C       This subroutine initializes the variables for the seeker model.
C
C       Inputs:
C           None.
C
C       Outputs:
C           None.
C
C       Internal variables and constants
C           None.
C
C       Begin math model initialization:
C
C       No initialization required.
C
        Return
        End

        Subroutine Seeker
C
C       This subroutine models a perfect one-axis seeker/tracker.
C
C       Inputs:
C           X_bi_i - X position of BCS w.r.t. ICS, expressed in the ICS [m]
C           Xd_bi_i - X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
C           Z_bi_i - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
C           Zd_bi_i - Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
C
C       Outputs:
C           Q_s - LOS rate [rad/sec]
C           Q_s_Meas - Measured LOS rate [rad/sec]
C
C       Internal variables and constants
C           None.
C
C       Declare global common and assign variable locations
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array(500), X_bi_i)
        Equivalence (Real_Array(501), Z_bi_i)
        Equivalence (Real_Array(502), Xd_bi_i)
        Equivalence (Real_Array(503), Zd_bi_i)
        Equivalence (Real_Array(600), Q_s)
        Equivalence (Real_Array(601), Q_s_Meas)
C
C       Begin math model:
C
C       Calculate true LOS rate
C
        Q_s = (Z_bi_i*Xd_bi_i - X_bi_i*Zd_bi_i)/
     &      (X_bi_i*X_bi_i + Z_bi_i*Z_bi_i)
C
C       Calculate measured LOS rate
C
        Q_s_Meas = Q_s
C
        Return
        End
