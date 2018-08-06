C
      Subroutine Print_Card
C
C     This subroutine decodes a "PRINT" card; which adds the specified
C     variable to the list of those to be printed.  The format is:
C
C     PRINT (Variable) (Location)
C
C     Inputs:
C        Number Of Prints - Number of communications array locations to print
C                           [Integer]
C        Tokens           - Parsed results from input line provided to the parser
C                           [Character*80 Array]
C
C     Outputs:
C        Number Of Prints - Number of communications array locations to print
C                           [Integer]
C        Print Index      - Communications array location of print data
C                           [Integer Array]
C
C     Internal variables and constants:
C        MAX NUMBER OF PRINTS - Maximum allowable number of communications array
C                               location to print (Integer]
C        Var_Loc              - Communications array index of variable to
C                               be printed (Integer]
C
C     Include common block definitions
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'parser.inc'
      Include 'print.inc'
C
C     Exceptions to default type
C
      Integer Var_Loc
C
C     If there's space left
C
      If (Number_Of_Prints .lt. MAX_NUMBER_OF_PRINTS) Then
C
C     Get location
C
Cjlc3    Decode((Index(Tokens(3), ' ') - 1), '(I5)', Tokens(3)) Var_Loc
         Read(UNIT=Tokens(3),FMT='(I5)') Var_Loc
C
C     Set global common index
C
         Number_Of_Prints = Number_Of_Prints + 1
         Print_Index(Number_Of_Prints) = Var_Loc
C
C     If there is no more space, write error message to output file
C
      Else
         Print *, ' Too many PRINT commands'
         Print *, ' PRINT command not executed'
      End If
C
C     Exit to Input_Data
C
      Return
      End
