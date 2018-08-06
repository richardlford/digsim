C
      Program DigSim
C
C     DigSim provides a FORTRAN architecture required to simulate
C     continuous systems described by sets of simultaneous first-order
C     differential equations.
C
C     Variables:
C       End_Of_Run      - Flag set by user to stop the simulation  [Logical]
C       Ndes_Real       - Number of real states [Integer]
C       ReEval_Derivs   - Indicates states changed value at a discrete event [Logical]
C       Stop_Simulation - Flag indicating simulation termination [sec]
C
C
C     Include common block variables and declarations
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'deriv.inc'
C
C     Exceptions to default type
C
      Logical End_Of_Run, Execute_Run
C
C       Open the input file
c
      Open(Unit=99, File='input.dat', Status='OLD')
C
C     Global driver initialization
C
      Call DigSim_Initialization
      Call Driver_Default_Data
C
C     Get data for first run
C
      Call Input_Data(Execute_Run)
C
C     If data is available, begin loop to execute simulation runs.
C
      Do While (Execute_Run)
C
C     Setup the driver for this run
C
         End_Of_Run = .FALSE.
         Call Zero_Global_Common
         Call Driver_Default_Data
         Call Default_Data
         Call Setup_Driver_For_Run
         Ndes_Real = 0
         Call Initialization
C
C     States at current time are known; calculate state derivatives at current time
C
         Call Differential_Equations
C
C     Process the initial discrete events
C
         Call Process_Events
C
C     Recalculate derivat:ves if so directed by user
C
         If (ReEval_Derivs) Then
            Call Differential_Equations
            ReEval_Derivs = .FALSE.
         End If
C
C     Advance the simulation states until termination.
C
         Do While (.not. (End_Of_Run .or. Stop_Simulation))
C
C     Advance states to new time
C
           Call Advance_States
C
C     Calculate state derivatives at new time
C
           Call Differential_Equations
C
C     Process discrete events based on current information
C
           Call Process_Events
C
C     Recalculate derivatives if so directed by user
C
           If (ReEval_Derivs) Then
              Call Differential_Equations
              ReEval_Derivs = .FALSE.
           End If
C
C     Check the termination conditions
C
           Call Termination_Conditions(End_Of_Run)
        End Do
C
C     Setup for next run
C
        Call Input_Data(Execute_Run)
C
C     End of run execution loop
C
      End Do
C
      Stop
      End
