C
      Subroutine Setup_Driver_For_Run
C
C     This subroutine sets up the driver and global common for the next run.
C
C     Inputs:
C        Number_Of_Real_Values - Current number of real variables to be
C                                initialized (Integer]
C        Set_Real_Value        - Initial values for real variables
C                                [Real Array]
C        Set_Real_Value_Index  - Real_Array locations for initial values
C                                [Integer Array]
C
C     Outputs:
C        Real_Array - The communications array for real variables [Real Array]
C
C     Internal variables and constants:
C        I - Loop index (Integer]
C
C     Include common block definitions
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Include '../driver/setval.inc'
C
C     Exceptions to default type
C
      Integer I
C
C     Initialize global common
C
      Do I = 1, Number_Of_Real_Values
         Real_Array(Set_Real_Value_Index(I)) = Set_Real_Value(I)
      End Do
C
      Return
      End
