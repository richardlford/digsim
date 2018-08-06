C
        Subroutine Default_Data
C
C   This subroutine sets the default data values.
C
C   Inputs:
C   None.
C
C   Outputs:
C       Damping_coefficient     - Damping force per velocity (N/m/s]
C       DtMax                   - Maximum allowable simulation time step [sec]
C       DtMin                   - Minimum allowable simulation time step [sec]
C       DtPrint                 - Time step between printing data [sec]
C       Gravity                 - Acceleration due to gravity [m/sec**2]
C       Mass                    - Mass suspended from spring [kg]
C       Spring_Coefficient      - Restoring force per position [N/m]
C       Time0                   - Initial time [sec]
C       Tstop                   - Simulation stop time [sec]
C       X_IC                    - Initial position of suspended mass [m]
C       Xd_IC                   - Initial velocity of suspended mass [m/sec]
C
C   Internal variables and constants:
C       None.
C
C   Declare global common
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array (10), Damping_Coefficient)
        Equivalence (Real_Array (11), Gravity)
        Equivalence (Real_Array (12), Mass)
        Equivalence (Real_Array (13), Spring_Coefficient)
        Equivalence (Real_Array (14), X_IC)
        Equivalence (Real_Array (15), Xd_IC)
C
C   Set model independent data and reset time
C
        DtMax   = 0.01e0
        DtMin   = 0.001e0
        DtPrint = 0.05e0
        Time0   = 0.0e0
        Tstop   = 2.5e0
C
C   Set the default data for the simulation
C
        Damping_Coefficient     =  8.88e0
        Gravity                 =  9.88e0
        Mass                    =  1.0e0
        Spring_Coefficient      = 39.47e0
        X_IC                    =  0.0e0
        Xd_IC                   =  0.0e0
C
        Return
        End
