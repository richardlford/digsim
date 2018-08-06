C
      Subroutine Discrete
C
C       This subroutine processes the scheduled user-defined discrete events.
C
C       Inputs:
C           Coef_Of_Restitution     - Coefficent of restitution [Real]
C           Xd                                              - Velocity of ball [m/sec]
C
C       Outputs:
C           ReEval_Derivs   - Indicates states changed value at a discrete event
C                                                 [Logical]
C     Xd                              - Velocity of ball [m/sec]
C
C       Internal variables and constants:
C           None.
C
C       Declare global common
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Include '../driver/event.inc'
      Equivalence (Real_Array(10), Coef_Of_Restitution)
      Equivalence (Real_Array(15), Xd)
C
C     Check for valid user event
C
      If (Events(1) .eq. 10) Then
         Xd = -Coef_Of_Restitution*Xd
      End If
      ReEval_Derivs = .TRUE.
C
      Return
      End
