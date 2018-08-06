C
      Subroutine Default_Data
C
C     This subroutine sets the default data values.
C
C     Inputs:
C       None.
C
C     Outputs:
C       DtMax                   - Maximum allowable simulation time step [sec]
C       DtMin                   - Minimum allowable simulation time step [sec]
C       DtPrint                 - Time step between printing data [sec]
C       Guidance_Gain   - Commanded angular rate per measured LOS rate [Real]
C       RDTODG                  - Radians to degrees conversion factor [deg/rad]
C       Time0                   - Initial time [sec]
C       Theta_IC_dg             - Initial attitude of missile [deg]
C       Tstop                   - Simulation stop time [sec]
C       Velocity                - Velocity of missile [m/sec]
C       X_IC                    - Initial X position of missile [m]
C       Z_IC                    - Initial Z position of missile [m]
C
C     Internal variables and constants
C       None.
C
C     Declare global common
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(10), RDTODG)
      Equivalence (Real_Array(12), Theta_IC_dg)
      Equivalence (Real_Array(13), Velocity)
      Equivalence (Real_Array(14), X_IC)
      Equivalence (Real_Array(15), Z_IC)
      Equivalence (Real_Array(25), Guidance_Gain)
C
C     Set model independent data and reset time
C

      DtMax   =  0.005e0
      DtMin   = 0.001e0
      DtPrint =  0.01e0
      Time0   =  0.0e0
      Tstop   = 10.0e0
      RDTODG  = 180.0e0/(4.0e0*atan(1.0e0))
C
C     Set the default data for the simulation
C
      Theta_IC_dg   =    0.0e0
      Velocity      =  100.0e0
      X_IC          = -500.0e0
      Z_IC          = -100.0e0
      Guidance_Gain =    3.0e0
C
      Return
      End
