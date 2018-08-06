C
      Subroutine Termination_Conditions(Quit)
C
C     Termination module
C
C     This module determines if the run termination conditions have been met
C
C     Inputs:
C       Z - Z position of missile [m]
C
C     Outputs:
C       Quit - Stop simulation run [Boolean]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Logical Quit
      Equivalence (Real_Array(17), Z)
C
      If (Z .ge. 0.0e0) Then
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
C       X - X position of missile [m]
C       Xd - X velocity of missile [m/sec]
C       Z  - Z position of missile [m]
C       Zd - Z velocity of missile [m/sec]
C
C     Outputs:
C       DtMiss - Time step to closest approach [sec]
C       RMiss  - Missile miss distance [m]
C
C     Internal variables and constants:
C       None.
C
C     Declare global comon and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(16), X)
      Equivalence (Real_Array(17), Z)
      Equivalence (Real_Array(19), Xd)
      Equivalence (Real_Array(20), Zd)
C
C     Begin miss distance calculation:
C
C     Evaluate closest approach time and miss distance
C
      DtMiss = (X*Xd + Z*Zd)/(Xd**2 + Zd**2)
      RMiss  = Sqrt((X - Xd*DtMiss)**2 + (Z - Zd*DtMiss)**2)
      Open(Unit = 98, File = 'miss.dat')
      Write(98, *) 'RMiss = ',RMiss
      Write(98, *) 'DtMiss = ',DtMiss
      Close(98)
C
      Return
      End
