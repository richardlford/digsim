C
        Subroutine Termination_Conditions(Quit)
C
C       This module determines if the run termination conditions have been met
C
C       Inputs:
C           X_bt_i  - X position of the BCS w.r.t. the target, expressed in the ICS [m]
C           Xd_bt_i - X velocity of the BCS w.r.t. the target, expressed in the ICS
C                             [m/sec]
C           Z_bt_i  - Z position of the BCS w.r.t. the target, expressed in the ICS [m]
C           Zd_bt_i - Z velocity of the BCS w.r.t. the target, expressed in the ICS
C                             [m/sec]
C
C       Outputs:
C           Quit - Stop simulation run [Boolean]
C
C       Internal variables and constants:
C           Time_To_Go - Time until closest approach [sec]
C
C       Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Logical Quit
      Equivalence (Real_Array(602), X_bt_i)
      Equivalence (Real_Array(603), Z_bt_i)
      Equivalence (Real_Array(604), Xd_bt_i)
      Equivalence (Real_Array(605), Zd_bt_i)
C
C       Begin consition check:
C
C       Evaluate closest approach time and miss distance
C
      Time_To_Go = -(X_bt_i*Xd_bt_i + Z_bt_i*Zd_bt_i)/
     &              (Xd_bt_i**2 + Zd_bt_i**2)
      If (Time_To_Go .lt. 0.0e0) Then
         Call Miss
         Quit = .TRUE.
      End If
C
      Return
      End

      Subroutine Miss
C
C       Miss distance calculation routine
C
C       Inputs:
C           X_bt_i  - X position of the BCS w.r.t. the target, expressed in the ICS [m]
C           Xd_bt_i - X velocity of the BCS w.r.t. the target, expressed in the ICS
C                             [m/sec]
C           Z_bt_i  - Z position of the BCS w.r.t. the target, expressed in the ICS [m]
C           Zd_bt_i - Z velocity of the BCS w.r.t. the target, expressed in the ICS
C                             [m/sec]
C
C       Outputs:
C           DtMiss  - Time step to closest approach [sec]
C           RMiss   - Missile miss distance [m]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations

      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(602), X_bt_i)
      Equivalence (Real_Array(603), Z_bt_i)
      Equivalence (Real_Array(604), Xd_bt_i)
      Equivalence (Real_Array(605), Zd_bt_i)
C
C       Begin miss distance calculation:
C
C       Evaluate closest approach time and miss distance
C
      DtMiss = (X_bt_i*Xd_bt_i + Z_bt_i*Zd_bt_i)/
     &         (Xd_bt_i**2     + Zd_bt_i**2)
      RMiss = Sqrt((X_bt_i - Xd_bt_i*DtMiss)**2 +
     &             (Z_bt_i - Zd_bt_i*DtMiss)**2)
      Open(Unit = 98, File = 'miss.dat')
      Write(98, *) 'RMiss = ',RMiss
      Write(98, *) 'DtMiss = ',Dtmiss
      Close(98)
C
      Return
      End
