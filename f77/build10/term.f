C
      Subroutine Termination_Conditions (Quit)
C
C     This module determines if the run termination conditions have been met.
C
C     Inputs:
C       X_tb_i  - Position of the target w.r.t. the BCS, expressed in the ICS [m]
C       Y_tb_i
C       Z_tb_i
C       Xd_tb_i - Velocity of the target w.r.t. the BCS, expressed in the ICS
C       Yd_tb_i   [m/sec]
C       Zd_tb_i
C
C     Outputs:
C       Quit - Stop simulation run [Boolean]
C
C     Internal variables and constants:
C       Time_To_Go - Time until closest approach [sec]
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Logical Quit
      Equivalence (Real_Array (606), X_tb_i)
      Equivalence (Real_Array (608), Y_tb_i)
      Equivalence (Real_Array (610), Z_tb_i)
      Equivalence (Real_Array (607), Xd_tb_i)
      Equivalence (Real_Array (609), Yd_tb_i)
      Equivalence (Real_Array (611), Zd_tb_i)
C
C     Begin consition check:
C
C     Evaluate closest approach time and miss distance
C
      Time_To_Go = -(X_tb_i*Xd_tb_i + Y_tb_i*Yd_tb_i + Z_tb_i*Zd_tb_i)
     &        /(Xd_tb_i**2 + Yd_tb_i**2 + Zd_tb_i**2)
      If (Time_To_Go .lt. 0.0e0) Then
        Call Miss
        Quit = .TRUE.
      End If
C
      Return
      End

      Subroutine Miss
C
C     Miss distance calculation routine
C
C     Inputs:
C       X_tb_i  - Position of the target w.r.t. the BCS, expressed n the ICS [m]
C       Y_tb_i
C       Z_tb_i
C       Xd_tb_i - Velocity of the target w.r.t. the BCS, expressed in the ICS
C       Yd_tb_i   [m/sec]
C       Zd_tb_i
C
C     Outputs:
C       DtMiss - Time step to closest approach [sec]
C       RMiss  - Missile miss distance [m]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array (606), X_tb_i)
      Equivalence (Real_Array (608), Y_tb_i)
      Equivalence (Real_Array (610), Z_tb_i)
      Equivalence (Real_Array (607), Xd_tb_i)
      Equivalence (Real_Array (609), Yd_tb_i)
      Equivalence (Real_Array (611), Zd_tb_i)
C
C     Begin miss distance calculation
C
C     Evaluate closest approach time and miss distance
C
      DtMiss = -(X_tb_i*Xd_tb_i + Y_tb_i*YD_tb_i + Z_tb_i*Zd_tb_i)/
     &          (Xd_tb_i**2 + Yd_tb_i**2 + Zd_tb_i**2)
      RMiss  = Sqrt((X_tb_i - Xd_tb_i*DtMiss)**2 +
     &              (Y_tb_i - Yd_tb_i*DtMiss)**2 +
     &              (Z_tb_i - Zd_tb_i*DtMiss)**2)
      Open (Unit = 98, File = 'miss.dat')
      Write (98, *) 'Rmiss  = ',Rmiss
      Write (98, *) 'DtMiss = ',Dtmiss
      Close (98)
C
      Return
      End
