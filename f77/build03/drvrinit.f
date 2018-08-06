C
      Subroutine DigSim_Initialization
C
C     This subroutine initializes all of the driver variables to before-use
C     conditions; i.e. counters or references are set to 0, etc.
C
C     Inputs:

C        MAX_NUMBER_OF-PRINTS      - Maximum allowable number of
C                                    communications array locations to
C                                    print [integer]
C        MAX NUMBER_OF_REAL_VALUES - Maximum allowable number of real
C                                    variables to be initialized [Integer]
C
C     Outputs:

C        Number_Of Prints - Number of communications array locations to
C                           print [Integer]
C
C        Number_Of_Real_Values - Current number of real variables to be
C                                initialized [Integer]
C
C        Print_Index           - Communications array location of print data
C                                [Integer Array]
C
C        Set_Real_Value        - Initial values for real variables [Real Array]
C        Set_Real_Value_Index  - Real_Array locations for initial values
C                                [Integer Array]
C
C
C     Internal variables and constants:
C        I - Loop index [Integer]
C
C     Include common block variables and declarations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Include '../driver/setval.inc'
      Include '../driver/print.inc'
C
C  Exceptions to default type
C
      Integer I
C
C     Initialize initial values common block variables
C
      Number_Of_Real_Values = 0
      Do I = 1, MAX_NUMBER_OF_REAL_VALUES
         Set_Real_Value(I) = 0.0
         Set_Real_Value_Index(I) = 0
      End Do
C
C     Initialize print common block variables
C
      Number Of Prints = 0
      Do I = 1, MAX_NUMBER_OF_PRINTS
         Print_Index(I) = 0
      End Do
C
      Return
      End
