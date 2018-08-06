C
      Subroutine Seeker_Data
C
C     This subroutine sets the default data for the seeker model.
C
C     Inputs:
C       None.
C
C     Outputs:
C       None.
C
C     Internal variables and constants:
C       None.
C
C     Begin default data definitions:
C       No default data required.
C
      Return
      End

      Subroutine Seeker_Init
C
C     This subroutine initializes the variables for the seeker model.
C
C     Inputs:
C       None.
C
C     Outputs:
C       None.
C
C     Internal variables and constant
C       None.
C
C     Begin math model initialization
C
C     No initialization required.
C
      Return
      End

      Subroutine Seeker

C     This subroutine models a perfect one-axis seeker/tracker.
C
C     Inputs:
C       X_bi_i  - X position of the BCS w.r.t. ICS, expressed in the ICS [m]
C       X_ti_i  - X position of the target, w.r.t. ICS in ICS[m]
C       Xd_bi_i - X velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
C       Xd_ti_i - X velocity of the target, w.r.t. ICS in ICS [m/sec]
C       Z_bi_i  - Z position of the BCS w.r.t. ICS, expressed in the ICS [m]
C       Z_ti_i  - Z position of the target, w.r.t. ICS in ICS [m]
C       Zd_bi_i - Z velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
C       Zd_ti_i - Z velocity of the target, w.r.t. ICS in ICS [m/sec]
C
C     Outputs:
C       Q_s             - LOS rate [rad/sec]
C       Q_s_Meas        - Measured LOS rate [rad/sec]
C       X_bt_i          - X position of the BCS w.r.t. ICS, expressed in the ICS
C                         [m]
C       Xd_bt_i         - X velocity of the BCS w.r.t. ICS, expressed in the ICS
C                         [m/sec]
C       Z_bt_i          - Z position of the BCS w.r.t. ICS, expressed in the ICS
C                         [m]
C       Zd_bt_i         - Z velocity of the BCS w.r.t. ICS, expressed in the ICS
C                         [m/sec]
C
C     Internal variables and constants:
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(500), X_bi_i)
      Equivalence (Real_Array(503), Z_bi_i)
      Equivalence (Real_Array(501), Xd_bi_i)
      Equivalence (Real_Array(504), Zd_bi_i)
      Equivalence (Real_Array(600), Q_s)
      Equivalence (Real_Array(601), Q_s_Meas)
      Equivalence (Real_Array(602), X_bt_i)
      Equivalence (Real_Array(603), Z_bt_i)
      Equivalence (Real_Array(604), Xd_bt_i)
      Equivalence (Real_Array(605), Zd_bt_i)
      Equivalence (Real_Array(700), X_ti_i)
      Equivalence (Real_Array(701), Xd_ti_i)
      Equivalence (Real_Array(702), Z_ti_i)
      Equivalence (Real_Array(703), Zd_ti_i)
C
C     Begin math model:
C
C     Target relative position and velocity in ICS
C
      X_bt_i  = X_bi_i - X_ti_i
      Z_bt_i  = Z_bi_i - Z_ti_i
      Xd_bt_i = Xd_bi_i - Xd_ti_i
      Zd_bt_i = Zd_bi_i - Zd_ti_i
C
C     Calculate true LOS rate
C
      Q_s = (Z_bt_i*Xd_bt_i - X_bt_i*Zd_bt_i)/
     &      (X_bt_i*X_bt_i + Z_bt_i*Z_bt_i)
C
C     Calculate measured L08 rate
C
      Q_s_Meas = Q_s
C
      Return
      End
