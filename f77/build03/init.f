C
      Subroutine Initialization
C
C     This routine initializes state values and state counters and pointers.
C
C     Inputs:
C        Time   - Simulation time [sec]
C        X      - Position of suspended mass [m]
C        X_IC   - Initial position of suspended mass [m]
C        Xd     - Velocity of suspended mass [m/sec]
C        Xd_IC  - Initial velocity of suspended mass [m/sec]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common asn assign variable locations
C
       Include '../driver/global.inc'
       Include '../driver/sysvars.inc'
       Include '../driver/deriv.inc'
       Equivalence (Real_Array(14), X_IC)
       Equivalence (Real_Array(15), Xd_IC)
       Equivalence (Real_Array(16), X)
       Equivalence (Real_Array(17), Xd)
C
C     Set initial time
C
       Time = Time0
C
C     Setup integrators
C
       Call Define_Real_State(16, 17)
       Call Define_Real_State(17, 18)
C
C     Initialize states
C
       X  = X_IC
       Xd = Xd_IC
C
       Return
       End
