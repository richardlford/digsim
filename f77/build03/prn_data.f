C
      Subroutine Print_Data
C
C     This subroutine writes print data to standard output.
C
C     Inputs:
C        Number_Of_Prints - Number of communications array locations to print
C                           [Integer]
C        Print_Index      - Communications array location of print data
C                           [Integer Array]
C        Real_Array       - The communications array for real variables
C                           [Real Array]
C        Time             - Simulation time (sec]
C
C     Outputs:
C        None.
C
C     Internal variables and constants:
C        I - Loop index [Integer]
C
C     Include common block definitions
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Include '../driver/print.inc'
C
C     Exceptions to default type
C
      Integer I
C
C     Write run data to standard output
C
      write(*, fmt="(ES13.5)", advance="no") Time
      Do i=1,Number_Of_Prints
          write(*, fmt="(ES13.5)", advance="no")
     &        Real_Array(Print_Index(I))
      end do
      Write(*,*)
C      print *,Time,
C     &     (Real_Array(Print_Index(I)), I = 1, Number_Of_Prints)
C
      Return
      End
