C
      Subroutine Default_Data
C
C     This subroutine calls the routines responsible for setting
C     default data values. It also sets default system values for this
C     simulation.
C
C     Inputs:
C       None.
C
C     Outputs:
C       Dt      - Integration time step [sec]
C       DtMax   - Maximum allowable Dt [sec]
C       DtMin   - Minimum allowable Dt [sec]
C       DtPrint - Print data logging interval [sec]
C       Time0   - Initial time [sec]
C       Tstop   - Simulation termination time [sec]
C
C     Internal variables and constants:
C       PI     - Ratio of a circle's circumference to its diameter [Real]
C       RDTODG - Radians to degrees conversion factor [deg/rad]
C       SMALL  - Arbitrarily small number [Real]
C
C     Declare global common
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array (50), PI)
      Equivalence (Real_Array (51), RDTODG)
      Equivalence (Real_Array (52), SMALL)
C
C     Set model independent data and reset time
C
      Dt      =  0.005e0
      DtMax   =  0.005e0
      DtMin   =  0.001e0
      DtPrint =  0.01e0
      Time0   =  0.0e0
      Tstop   = 10.0e0
C
C       Commonly used constants
C
      PI     = 4.0e0*Atan (1.0e0)
      RDTODG = 180.0e0/PI
      SMALL  = 1.0e-6
C
C     Call routines to set default data
C
      Call Actuator_Data
      Call Aerodynamics_Data
      Call Mass_Data
      Call Kinematics_Data
      Call Gyro_Data
      Call Target_Data
      Call Seeker_Data
      Call Flight_Computer_Data
C
      Return
      End
