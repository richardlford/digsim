C
      Program DigSim
C  DigSim provides a FORTRAN architecture required to simulate
C  continuous systems described by sets of simultaneous first-order
C  differential equations.
C
C  Variables:
C    End Of Run - Flag set by user to stop the s{mulation [Logical]
C
C
C  Exceptions to default type
C
      Logical End_Of_Run
C
C  Set the default data for this run
C
      Call Default_Data
C
C  Setup & initialize the states
C
      Call Initialization
C
C  Determine the inital value for the state derivatives
C
      Call Differential_Equations
C
C  Get the initial value for the run termination flag
C
      Call Termination_Conditions(End_Of_Run)
C
C  Advance the simulation states until termination
C
      Do While (.not. End_Of_Run)
C
C  Advance states to new time
C
         Call Advance_States
C
C  Calculate state derivatives at new time
C
         Call Differential_Equations
C
C  Check for end of run
C
         Call Termination_Conditions(End_Of_Run)
C
C  End of main simulation loop
C
      End Do
C
      Stop
      End
