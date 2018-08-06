C
      Subroutine Setup_Driver_For_Run
C
C     This subroutine sets up the driver and global common for the next run..
C
C     Inputs:
C       MAX_NUMBER_OF_EVENTS  - Maximum allowable number of events in the event queue [Integer]
C       Number_Of_Prints      - Number of communications array locations to print [Integer]
C       Number_Of_Real_Values - Current number of real variables to be initialized [Integer]
C       Set_Real_Value        - Initial values for real variables [Real Array]
C       Set_Real_Value_Index  - Real_Array locations for initial values [Integer Array]
C
C     Outputs:
C       LOG_PRINT_DATA        - Event corresponding to printing data to standard output file [Integer]
C       Number_Of_Events      - Current number of events in the event queue [Integer]
C       Events                - Event queue [Integer Array]
C       Stop_Simulation       - Flag indicating simulation termination [sec]
C       TERMINATE_SIMULATION  - Event corresponding to simulation termination [Integer]
C       Time_Of_Events        - Time to execute corresponding event in event queue [Real Array]
C       TStop                 - Simulation termination time [sec]
C       Real_Array            - The communications array for real variables [Real Array]
C
C     Internal variables and constants:
C       I                     - Loop index [Integer]
C
C     Include common block definitions
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'setval.inc'
      Include 'print.inc'
      Include 'event.inc'
C
C     Event designations
C
      Include 'event.prm'
C
C     Exceptions to default type
C
      Integer I
C
C     Flush the event queue
C
      Number_Of_Events = 0
      Do I = 1, MAX_NUMBER_OF_EVENTS
         Events (I)        = 0
         Time_Of_Events(I) = 1.0e10
      End Do
C
C     Initialize global common
C
      Do I = 1, Number_Of_Real_Values
         Real_Array (Set_Real_Value_Index(I)) = Set_Real_Value(I)
      End Do
C
C     Schedule initial print
C
      If (Number_Of_Prints .gt. 0) Then
         Call schedule (0.0e0, LOG_PRINT_DATA)
      End If
C
C     Initialize termination flag
C
      Stop_Simulation = .FALSE.
      Call Schedule (TStop, TERMINATE_SIMULATION)
C
      Return
      End
