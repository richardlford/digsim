C
        Subroutine Termination_Conditions(Quit)
C
C   Termination module
C
C   Inputs:
C     Z_bi_i - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
C
C   Outputs:
C     Quit - Stop simulation run [Boolean]
C
C   Internal variables and constants:
C     None.
C
C   Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Logical Quit
      Equivalence (Real_Array(501), Z_bi_i)
C
      If (Z_bi_i .ge. 0.0e0) Then
        Call Miss
        Quit = .TRUE.
      End If
C
      Return
      End

      Subroutine Miss
C
C   Miss distance calculation routine
C
C  Inputs:
C    X_bi_i  - X position of BCS w.r.t. ICS, expressed in the ICS [m]
C    Xd_bi_i - X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
C    Z_bi_i  - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
C    Zd_bi_i - Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
C
C  Outputs:
C    DtMiss - Time step to closest approach [sec]
C    RMiss  - Missile miss distance [m]
C
C  Internal variables and constants:
C    None.
C
C  Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(500), X_bi_i)
      Equivalence (Real_Array(501), Z_bi_i)
      Equivalence (Real_Array(502), Xd_bi_i)
      Equivalence (Real_Array(503), Zd_bi_i)
C
C  Begin miss distance calculation:
C
C  Evaluate closest approach time and miss distance
C
      DtMiss = (X_bi_i*Xd_bi_i + Z_bi_i*Zd_bi_i)/
     &         (Xd_bi_i**2 + Zd_bi_i**2)
      RMiss  = Sqrt((X_bi_i - Xd_bi_i*DtMiss)**2 +
     &              (Z_bi_i - Zd_bi_i*DtMiss)**2)
      Open(Unit = 98, File = 'miss.dat')
      Write(98, *) 'Rmiss  = ',Rmiss
      Write(98, *) 'DtMiss = ',Dtmiss
      Close(98)
C
      Return
      End
