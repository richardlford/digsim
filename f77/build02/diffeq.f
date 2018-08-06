C
      Subroutine Differential_Equations
C     This routine evaluates the state derivatives for the
C     mass-spring-damper system.
C
C     Inputs:
C       Damping_Coefficient - Damping force per velo'city [N/m/s]
C       Gravity             - Acceleration due to gravity [m/sec**2]
C       Mass                - Mass suspended from spring [Kg]
C       Spring_Coefficient  - Restoring force per position [N/m]
C       X                   - Position of suspended mass (m]
C       Xd                  - Velocity of suspended mass [m/sec]
C
C     Outputs:
C       Xdd - Acceleration of suspended mass [m/sec**2]
C
C     Internal variables and constants:
C       None.
C
C     Declare global common
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Equivalence (Real_Array(10), Damping_Coefficient)
      Equivalence (Real_Array(11), Gravity)
      Equivalence (Real_Array(12), Mass)
      Equivalence (Real_Array(13), Spring_Coefficient)
      Equivalence (Real_Array(16), X)
      Equivalence (Real_Array(17), Xd)
      Equivalence (Real_Array(18), Xdd)
C
C  Print states for this time
C
 100     Format(' ',3ES15.5)
         Write(*,100) Time, X, Xd
C
C       Calculate derivative at current time
C
      Xdd = -(Spring_Coefficient*X + Damping_Coefficient*Xd)/Mass -
     &     Gravity
C
      Return
      End
