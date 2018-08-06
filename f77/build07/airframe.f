C
        Subroutine Airframe_Response_Data
C
C       This subroutine sets the default data for determining the airframe response
C       to a rate command.
C
C       Inputs:
C           None.
C
C       Outputs:
C           None.
C
C       Internal variables and constants:
C           None.
C
C       Begin default data definitions:
C           No default data required.
C
        Return
        End

        Subroutine Airframe_Response_Init
C
C       This subroutine initializes the variables for determining the airframe
C       response to a rate command.
C
C       Inputs:
C           None.
C
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

        Subroutine Airframe_Response
C
C       This subroutine determines the airframe response to a rate command.
C
C       Inputs:
C           ThetaDot_b_Cmd - Commanded attitude rate of missile [rad/sec]
C
C       Outputs:
C           ThetaDot_b - Attitude rate of missile [rad/sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array(300), ThetaDot_b_Cmd)
        Equivalence (Real_array(505), ThetaDot_b)
C
C       Begin math model:
C
C       Calculate airframe rate
C
        ThetaDot_b = ThetaDot_b_Cmd
C
        Return
        End
