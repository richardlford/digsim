C
        Subroutine Initialization
C
C       This routine calls the routines to initialize state values, state counters
C       and pointers.
C
C       Inputs:
C           Time0 - Initial time [sec] ' '
C
C       Outputs:
C           Time - Simulation time [sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
C
C       Set initial time
C
        Time = Time0
C
C       Initialize models
C
        Call Kinematics_Init
        Call Seeker_Init
        Call Flight_Computer_Init
        Call Airframe_Response_Init
C
        Return
        End
