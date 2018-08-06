C
        Subroutine Flight_Computer_Data
C
C       This subroutine sets the default data for the flight computer.
C
C       Inputs:
C           None.
C
C       Outputs:
C           Guidance_Gain - Commanded angular rate per measured LOS rate [Real]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array(301), Guidance_Gain)
C
C       Begin default data definitions:
C
        Guidance_Gain = 3.0e0
C
        Return
        End

        Subroutine Flight_Computer_Init
C
C       This subroutine initializes the variables for the flight computer.
C
C       Inputs:
C           None.
C       Outputs:
C           None.
C
C       Internal variables and constants:
C           None.
C
C       Begin math model initialization:
C
C       No initialization required.
C
        Return
        End

        Subroutine Flight_Computer
C
C       This subroutine determines the commanded airframe rate.
C
C       Inputs:
C           Guidance_Gain - Commanded angular rate per measured LOS rate (Real)
C           Q_s_Meas - Measured LOS rate [rad/sec]
C
C       Outputs:
C           ThetaDot_b_Cmd - Commanded attitude rate of missile [rad/sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array(300), ThetaDot_b_Cmd)
        Equivalence (Real_Array(301), Guidance_Gain)
        Equivalence (Real_Array(601), Q_s_Meas)
C
C       Begin math model:
C
C       Calculate guidance command
C
        ThetaDot_b_Cmd = Guidance_Gain*Q_s_Meas
C
        Return
        End
