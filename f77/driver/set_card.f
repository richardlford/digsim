C
      Subroutine Set_Card
C
C     This subroutine decodes a "SET" card; which initializes the specified
C     global common location to the specified value. The format is:
c
C     SET (Variable) = (Value) (Location)
C
C     Inputs:
C       Tokens - Parsed results from input line provided to the
C                parser [Character*80 Array]
C
C     Outputs:
C       Number_Of_Real_Values - Current number of real variables to be initialized
C                               [Integer]
C
C       Set_Real_Value        - Initial values for real variables [Real Array]
C       Set_Real_Value_Index  - Real_Array locations for initial values
C                               [Integer Array]
C
C
C     Internal variables and constants:       1
C       MAX_NUMBER_OF_REAL_VALUES - Maximum allowable number of real variables
C                                   to be initialized [Integer]
C       Var_Loc                   - Location of variable  [Integer]
C
C     Include common block definitions
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'parser.inc'
      Include 'setval.inc'
C
C     Exceptions to default type
C
      Integer Var_Loc
C
C     If there's space left...
C
      If (Number_Of_Real_Values .lt. MAX_NUMBER_OF_REAL_VALUES) Then
C
C     Get location
C
C     jlc3    Decode((Index(Tokens(4), ' ') - 1), '(I5)', Tokens(4)) Var_Loc
         Read(UNIT=Tokens(4), FMT='(I5)') Var_Loc
C
C     Store global common location & initial value
C
         Number_Of_Real_Values = Number_Of_Real_Values + 1
         Set_Real_Value_Index(Number_Of_Real_Values) = Var_Loc
Cjlc3    Decode((Index(Tokens(3), ' ') - 1), '(F16.8)',  Tokens(3))
Cjlc3     &        Set_Real_Value(Number_Of_Real_Values)
         Read(UNIT=Tokens(3),FMT='(F16.8)')
     &        Set_Real_Value(Number_Of_Real_Values)
C
      Else
         Print *, ' Too many SET commands'
         Print *, ' SET command not executed'
      End If
C
C     Exit to Input_Data
C
      Return
      End
