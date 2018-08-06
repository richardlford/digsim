C
      Subroutine Gyro_Data
C
C     Angular rate sensors model default data
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
C     Begin math model:
C
C     No default data required.
C
      Return
      End

      Subroutine Gyro_Init
C
C     Angular rate sensors model initialization
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
C     Begin math model initialization
C
C     No initialization required.
C
      Return
      End

      Subroutine Gyro
C
C     This model simulates signals from three axis of ideal rate gyros.
C
C     Inputs:
C       P_b - Missile inertial angular velocity [rad/sec]
C       Q_b
C       R_b
C
C     Outputs:
C       P_g_Meas  - Measured angular velocity [rad/sec]
C       R_g_Meas
C       Q_g_Meas
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(200), P_g_Meas)
      Equivalence (Real_Array(201), Q_g_Meas)
      Equivalence (Real_Array(202), R_g_Meas)
      Equivalence (Real_Array(509), P_b)
      Equivalence (Real_Array(511), Q_b)
      Equivalence (Real_Array(513), R_b)
C
C     Attitude rate signals
C
      P_g_Meas = P_b
      Q_g_Meas = Q_b
      R_g_Meas = R_b
C
      Return
      End
