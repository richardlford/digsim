C
        Subroutine Differential_Equations
C
C    This routine calls the modules that evaluate the state derivatives for the
C    terminal homing simulation.
C
        Call Kinematics
        Call Seeker
        Call Flight_Computer
        Call Airframe_Response
C
        Return
        End
