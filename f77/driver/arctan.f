C
      Real Function Arctan (Y, X)
C
C     This function computes the arctangent of Y/X. 0 is returned for
C     an input of 0/0.
C
C     Inputs:
C       X - In a right triangle, the ratio of the side adjacent to the angle
C           to the side opposite the angle X [Real]
C
C     Outputs:
C       Arctan - Output angle [rad]
C
C     Internal variables and constants:
C       None.
C
      Implicit None
      Real X,
     &     Y
C
      If ((X .eq. 0.0e0) .and. (Y .eq. 0.0e0)) Then
        Arctan = 0.0e0
      Else
        Arctan = Atan2 (Y, X)
      End If
C
      Return
      End
