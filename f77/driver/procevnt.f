C
      Subroutine Process_Events
C
C     This subroutine processes the various discrete events. If the
C     event is a system event, the appropriate system routine is called;
C     if it is a user event (i.e. greater than 9) the user discrete
C     processing routine, Descrete, is called. The user is responsible
C     for monitoring for illegal user events.
C
C     Inputs:
C        DtMax                  - Maximum allowable integration step size [sec]
C        DtMin                  - Minimum allowable integration step size [sec]
C        Events                 - Event queue [Integer Array]
C        Number_Of_Events       - Current number of events in the event queue [Integer]
C        Time                   - Simulation time [sec]
C        Time_Of_Events         - Time to execute corresponding event in event queue [Real Array]
C
C     Outputs:
C        Dt                     - Integration step size [sec]
C        Stop_Simulation        - Flag indicating simulation termination [sec]
C
C     Internal variables and constants:
C        LOG_PRINT_DATA         - Event corresponding to printing data to standard output file [Integer]
C        More_Events            - Flag indicating that the remaining events are scheduled
C                                 for more than DtMax from now (Logical)
C        TERMINATE_SIMULATION   - Event corresponding to simulation termination [Integer]
C        Time_To_Next_Event     - Unadjusted time step to next discrete event [Real]
C
C     Include common block variables and declarations
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'event.inc'
C
C   Event parameter definitions
C
      Include 'event.prm'
C
C     Exceptions to default type
C
      Logical More_Events
C
C     Process events as long as there are events remaining within this time step
C
      If (Number_Of_Events .gt. 0) Then
         Do While ((Time_Of_Events(1) - Time) .lt. DtMin)
C
C     Process current event
C     Write out data to standard output
C
            If (Events(1) .eq. LOG_PRINT_DATA) Then
               Call Print_Data
C
C     Check for simulation termination
C
            Else If (Events(1) .eq. TERMINATE_SIMULATION) Then
               Stop_Simulation = .TRUE.
C
C     Check for user event
C
            Else If (Events(1) .ge. 10) Then
               Call Discrete
C
C     If no match to above cases, write error message and go to next event.
C
            Else
               Write (*, '(A)') ' Undefined system event scheduled'
            End If
C
C     Remove event from queue and process next event
C
            Call Remove_Event
         End Do
C
C     Adjust Dt as necessary
C
         Time_To_Next_Event = Time_Of_Events(1) - Time
         More_Events        = Number_Of_Events .gt. 0
         If ((Time_To_Next_Event .lt. DtMax) .and. More_Events) Then
            Dt = Time_To_Next_Event
         Else
            Dt = DtMax
         End If
      Else
         Dt = DtMax
      End If
C
      Return
      End
