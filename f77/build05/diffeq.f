C
      Subroutine Differential_Equations
C
C     This routine evaluates the state derivatives for a bouncing ball.
C
C     Inputs:
C       Gravity - Acceleration due to gravity [m/sec**2]
C       X               - Position of ball [m]
C       Xd              - Velocity of ball [m/sec]
C
C     Outputs:
C
C       Xdd - Acceleration of ball [m/sec**2]
C
C     Internal variables and constants:
C       Dtlmpact - Time until impact [Sec]
C
C     Declare global common
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(11), Gravity)
      Equivalence (Real_Array(14), X)
      Equivalence (Real_Array(15), Xd)
      Equivalence (Real_Array(16), Xdd)
C
C     Calculate acceleration at current time
C
      Xdd = -Gravity
C
C     Check to see if impact will occur within a maximum time step
C
      If ((X + Xd*DtMax + 0.5*Xdd*DtMax*DtMax) .le. 0.0e0) Then
C
C     If so, check to see if it is too late
C
         If ((X + Xd*DtMin + 0.5*Xdd*DtMin*DtMin) .le. 0.0e0) Then
C
C     If too late, set delay until impact to zero
C
            DtImpact = 0.0e0
C
C     Otherwise, calculate delay until actual impact
C
         Else
            DtImpact = (-Xd - Sqrt(Xd*Xd - 2.0e0*X*Xdd))/(2.0e0*X)
            If (DtImpact .lt. DtMin) Then
               DtImpact = (-Xd + Sqrt(Xd*Xd - 2.0e0*X*Xdd))/(2.0e0*X)
            End If
         End If
C
C     Schedule impact
C
         Call Schedule((Time + DtImpact), 10)
      End If
C
      Return
      End
