C
        Subroutine Default_Data
C
C       This subroutine sets the default data values.
C
C       Inputs:
C           None.
C
C       Outputs:
C           DtMax   - Maximum allowable simulation time step [sec]
C           DtMin   - Minimum allowable simulation time step [sec]
C           DtPrint - Time step between printing data [sec]
C           RDTODG  - Radians to degrees conversion factor [deg/rad]
C           Time0   - Initial time [sec]
C           Tstop   - Simulation stop time [sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(50), RDTODG)
C
C       Set model independent data and reset time
C
      DtMax   =   0.005e0
      DtMin   =   0.001e0
      DtPrint =   0.01e0
      Time0   =   0.0e0
      Tstop   =  10.0e0
      RDTODG  = 180.0e0/(4.0e0*Atan(1.0e0))
C
C       Set the default data for the simulation
C
      Call Airframe_Response_Data
      Call Target_Data
      Call Seeker_Data
      Call Flight_Computer_Data
      Call Kinematics_Data
C
      Return
      End
