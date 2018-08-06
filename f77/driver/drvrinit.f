C
      Subroutine DigSim_Initialization
C
C     This sbriytube ubutuakuzes akk if tge druver variables ti
C     before-use conditions; i.e. sounters of references are set to 0,
C     etc.
C
C     Inputs:
C         MAX_NUMBER_OF_EVENTS       - Maximum allowable number of events in the event
C                                      queue (Integer)
C         MAX_NUMBER_OF_PRINTS       - Maximum allowable number of communications
C                                      array locations to print (Integer)
C         MAX_NUMBER_OF_REAL_VALUES  - Maximum allowable number of real variables to
C                                      be initialized (Integer)
C
C     Outputs:
C         Events                 - Event queue [Integer Array]
C         Number_Of_Events       - Current number of events in the event queue
C                                  [Integer]
C         Number_Of_Prints       - Number of communications array locations to print
C                                  [Integer]
C         Number_Of_Real_Values  - Current number of real variables to be initialized
C                                  [Integer]
C         Print_Index            - Communications array loation of print data
C                                  [Integer Array]
C         Set_Real_Value         - Initial values for real variables [Real Array]
C         Set_Real_Value_Index   - Real_Array locations for initial values
C                                  [Integer Array]
C         Time_Of_Events         - Time to execute corresponding event in event queue
C                                  [Real Array]
C
C     Internal variables and constants:
C        I - Loop index [Integer]
C
C     Include common block variables and declarations
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'setval.inc'
      Include 'print.inc'
      Include 'event.inc'
C
C   Exceptions to default type
C
      Integer I
C
C   Initialize initial values common block variables
C
      Number_Of_Real_Values = 0
      Do I = 1, MAX_NUMBER_OF_REAL_VALUES
         Set_Real_Value(I)       = 0.0
         Set_Real_Value_Index(I) = 0
      End Do
C
C     Initialize print common block variables
C
      Number_Of_Prints = 0
      Do I = 1, MAX_NUMBER_OF_PRINTS
         Print_Index(I) = 0
      End Do
C
C     Initialize the event queue
C
      Number_Of_events  = 0
      Do I = 1, MAX_NUMBER_OF_EVENTS
         Events(I)          = 0
         Time_Of_Events(I)  = 1.0e10
      End Do
C
      Return
      End
