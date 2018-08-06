C
      Subroutine Print_Data
C
C     This subroutine writes print data to standard output.
C
C     Inputs:
C        DtPrint          - Time step between printing data [sec]
C        Number_Of_Prints - Number of communications array locations to print [Integer]
C        Print_Index      - Communications array location of print data [Integer Array]
C        Real_Array       - The communications array for real variables [Real Array]
C        Time             - Simulation time [sec]
C
C     Outputs:
C        LOG_PRINT_DATA   - Event corresponding to printing data to standard output file [Integer]
C
C     Internal variables and constants:
C        I                - Loop index [Integer]
C
C     Include common block definitions
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'print.inc'
C
C     Event designation parameters
C
      Include 'event.prm'
C
C     Exceptions to default type
C
      Integer I
C
C       Write run data to standard output
C
      write(*, fmt="(ES13.5)", advance="no") Time
      Do i=1,Number_Of_Prints
          write(*, fmt="(ES13.5)", advance="no")
     &        Real_Array(Print_Index(I))
      end do
      Write(*,*)
C
C     Schedule next print
C
      Call Schedule(((Nint(Time/DtPrint) + 1)*DtPrint),LOG_PRINT_DATA)
C
      Return
      End
