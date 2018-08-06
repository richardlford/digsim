      Subroutine Initialization
C
C    This routine initializes state values and state counters and pointers
C
C    Inputs:
C           Time0     - Initial time [sec]
C
C    Outputs:
C
C        Time    - Simulation time [sec]
C        X        - Position of ball [m]
C        X_IC    - Initial position of ball [m]
C        Xd        - Velocity of ball [m/sec]
C        Xd_IC    - Initial velocity of ball [m/sec]
C
C    Internal variables and constants:
C        None.
C
C    Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Include '../driver/deriv.inc'
      Equivalence (Real_Array(12), X_IC)
      Equivalence (Real_Array(13), Xd_IC)
      Equivalence (Real_Array(14), X)
      Equivalence (Real_Array(15), Xd)
C
C     Set initial time
C
      Time = Time0
C
C     Setup integrators
C
      Call Define_Real_State(14, 15)
      Call Define_Real_State(15, 16)
C
C     Initialize states
C
      X  = X_IC
      Xd = Xd_IC
C
      Return
      End
