      Subroutine Initialization
C
C     This routine initializes state values and state counters and pointers;
C     then calls the routines to initialize the various models.
C
C     Inputs:
C       Time0 - Initial time [sec]
C
C     Outputs:
C       Time - Simulation time [sec]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
C
C     Set initial time
C
      Time = Time0
C
C     Initialize models
C
      Call Actuator_Init
      Call Aerodynamics_Init
      Call Mass_Init
      Call Kinematics_Init
      Call Gyro_Init
      Call Target_Init
      Call Seeker_Init
      Call Flight_Computer_Init
C
      Return
      End
