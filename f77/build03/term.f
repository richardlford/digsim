C
       Subroutine Termination_Conditions(Quit)
C
C      Termination module
C
C      This module determines if the run termination conditions have been met
C
C      Inputs:
C        Time - Simulation time [sec]
C        Tatpr = Simulation termination time [sec]
C
C      Outputs:
C        Quit - Stop simulation run [Boolean]
C
C      Internal variables and constants:
C        None.
C
C      Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Logical Quit
C
      Quit = Time  .ge.  Tstop
C
      Return
      End
