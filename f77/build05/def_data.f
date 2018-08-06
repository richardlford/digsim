C
        Subroutine Default_Data
C
C       This subroutine set a the default data values.
C
C       Inputs:
C           None.
C
C       Output:
C           Coef_Of_Restitution - Coefficent of restitution [Real]
C           DtMax               - Maximum allowable simulation time step [sec]
C           DtMin               - Minimum allowable simulation time step [sec]
C           DtPrint             - Time step between printing data [sec]
C           Gravity             - Acceleration due to gravity [m/sec**2]
C           Time0               - Initial time [sec]
C           Tstop               - Simulation stop time [sec]
C           X_IC                - Initial position of ball [m]
C           XD_IC               - Initial velocity of ball [m/sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence(Real_Array(10), Coef_Of_Restitution)
        Equivalence(Real_Array(11), Gravity)
        Equivalence(Real_Array(12), X_IC)
        Equivalence(Real_Array(13), Xd_IC)
C
C       Set model independent data and reset time
C
        Dtmax   = 0.005e0
        DtMin   = 0.001e0
        DtPrint = 0.01e0
        Time0   = 0.0e0
        Tstop   = 10.0e0
C
C       Set the default data for the simulation
C
        Coef_Of_Restitution =  0.80e0
        Gravity             =  9.88e0
        X_IC                = 10.0e0
        Xd_IC               =  0.0e0
C
        Return
        End
