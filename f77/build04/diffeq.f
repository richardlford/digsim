C
        Subroutine Differential_Equations
C
C  This routine evaluates the state derivatives for the mass-spring-damper system.
C
C  Inputs:
C    Damping_Coefficient     - Damping force per velocity [N/m/s]
C    Gravity                 - Acceleration due to gravity [m/sec**2]
C    Mass                    - Mass suspended from spring [kg]
C    Spring_Coefficient      - Restoring force per position [N/m]
C    X                       - Position of suspended mass [m]
C    Xd                      - Velocity of suspended mass [m/sec]
C
C  Outputs:
C    Xdd - Acceleration of suspended mass [m/sec**2]
C
C  Internal variables and constants:
C    None.
C
C  Declare global common
C
        Include '../driver/global.inc'
        Include '../driver/sysvars.inc'
        Equivalence (Real_Array (10), Damping_Coefficient)
        Equivalence (Real_Array (11), Gravity)
        Equivalence (Real_Array (12), Mass)
        Equivalence (Real_Array (13), Spring_Coefficient)
        Equivalence (Real_Array (16), X)
        Equivalence (Real_Array (17), Xd)
        Equivalence (Real_Array (18), Xdd)
C
C       Calculate derivative at current time
C
        Xdd = -(Spring_Coefficient*X + Damping_Coefficient*Xd) /Mass -
     &        Gravity
C
        Return
        End
