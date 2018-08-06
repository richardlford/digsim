C
      Subroutine Advance_States
C
C  This subroutine solves differential equation initial value problems;
C  note that the initial derivatives are assumed to have been evaluated
C  prior to the subroutine call.
C
C
C  Inputs:
C     Dt         - Integration step size [sec]
C     Ix_Real    - Real_Array locations of states [Integer Array]
C     Ndes_Real  - Number of real states [Integer]
C     Real_Array - The communications array for real variables [Real Array]
C
C  Outputs:
C     Real_Array - The communications array for real variables [Real Array)
C     Time       - Simulation time [sec]
C
C  Internal variables and constants:
C     I - Loop index [Integer]
C
C  Include files
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'deriv.inc'
C
C  Exceptions to default type
C
      Integer I
C
C  Use Euler method to advance states one time step
C
      Do I = 1, Ndes_Real
         Real_Array(Ix_Real(I)) = Real_Array(Ix_Real(I)) +
     &                            Real_Array(IxDot_Real(I))*Dt
      End Do
C
C  Advance time to match states
C
      Time = Time + Dt
C
      Return
      End
