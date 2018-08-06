C
      Subroutine Zero_Global_Common
C
C     This subroutine sets the global common to all zero's.
C
C     Inputs:
C       None.
C
C     Outputs:
C       Real_Array      - The communications array for logical variables [Logical Array]
C
C     Internal variables and constants:
C       I               - Loop index [Integer]
C       REAL_ARRAY_SIZE - Maximum allowable real communications array index [Integer]
C
C     Include common block definitions
C
      Include 'global.inc'
C
C     Exceptions to default type
C
      Integer I
C
C     Zero global common
C
      Do I = 1, REAL_ARRAY_SIZE
         Real_Array (I) = 0
      End Do
C
      Return
      End
