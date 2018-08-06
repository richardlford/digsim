C
      Subroutine Driver_Default_Data
C
C     This subroutine sets the default values for the user-accessable
C     system variables.
C
C     Inputs:
C        None.
C
C     Outputs:
C        Dt    - Integration step size [sec]
C        Time  - Simulation time [sec]
C        Time0 - Initial time [sec]
C        TStop - Simulation termination time [sec]
C
C     Internal variables and constants:
C        None.
C
C     Declare global common
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
C
C     Set system default values
C
      Time   = 0.0e0
      Time0  = 0.0e0
      TStop  = 0.0e0
      Dt     = 0.00e0
C
      Return
      End
