C
      Subroutine Initialization
C
C  This routine initializes state values and state counters and
C  pointers.
C

C  Inputs:
C     Time0 - Initial time [sec]
C
C  Outputs:
C     Time  - Simulation time [sec]
C     X     - Position of suspended mass [m]
C     X_IC  - Initial position of suspended mass [m]
C     Xd    - Velocity of suspended mass [m/sec]
C     Xd_IC - Initial velocity of suspended mass [m/sec]
C
C  Internal variables and constants:
C     None.
C
C Declare global common and assign variable locations
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'deriv.inc'
      Equivalence (Real_Array(14), X_IC)
      Equivalence (Real_Array(15), Xd_IC)
      Equivalence (Real_Array(16), X)
      Equivalence (Real_Array(17), Xd)
C
C  Set initial time
C
      Time = Time0
C
C  Setup integrators
C
      Ndes_Real     = 2
      Ix_Real(1)    = 16
      IxDot_Real(1) = 17
      Ix_Real(2)    = 17
      IxDot_Real(2) = 18
C
C  Initialize states
C
      X  = X_IC
      Xd = Xd_IC
C
      Return
      End
