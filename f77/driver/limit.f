C
      Real Function Limit(X, Lower_Limit, Upper_Limit)
C
C     This function bounds X between Lower_Limit and Upper_Limit.
C
C     Inputs:
C       Lower_Limit - Minimum allowable value for Value [Real]
C       Upper_Limit - Maximum allowable value for Value [Real]
C       X           - The value to be limited [Real]
C
C     Outputs:
C       Limit  -  Bounded value of X [Real]
C
C     Internal variables and constants:
C       None.
C
      Implicit None
      Real Lower_Limit,
     &     Upper_Limit,
     &     X
C
      If (X .lt. Lower_Limit) Then
        Limit = Lower_Limit
      Else If (X .gt. Upper_Limit) Then
        Limit = Upper_Limit
      Else
        Limit = x
      End If
C
      Return
      End
