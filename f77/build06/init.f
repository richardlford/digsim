C
      Subroutine Initialization
C
C     This routine initializes state values and state counters and pointers.
C
C     Inputs:
C       RDTODG      - Radians to degrees conversion factor [deg/rad]
C       Theta_IC_dg - Initial attitude of missile [deg]
C       Time0       - Initial time [sec]
C       X_IC        - Initial X position of missile [m]
C       Z_IC        - Initial Z position of missile [m]
C
C     Outputs:
C       X           - X position of missile [m]
C       Z           - Z position of missile [m]
C       Theta       - Attitude of missile [rad]
C       Time        - Simulation time [sec]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Include '../driver/deriv.inc'
      Equivalence (Real_Array(10), RDTODG)
      Equivalence (Real_Array(12), Theta_IC_dg)
      Equivalence (Real_Array(14), X_IC)
      Equivalence (Real_Array(15), Z_IC)
      Equivalence (Real_Array(16), X)
      Equivalence (Real_Array(17), Z)
      Equivalence (Real_Array(19), Theta)
C
C     Set initial time
C
      Time = Time0
C
C     Setup integrators
C
      Call Define_Real_State(16, 19)
      Call Define_Real_State(17, 20)
      Call Define_Real_State(18, 21)
C
C     Initialize states
C
      X     = X_IC
      Z     = Z_IC
      Theta = Theta_IC_dg/RDTODG
C
      Return
      End
