C
      Subroutine Input_Data(Execute_Run)
C
C     This subroutine reads the input data cards; then calls the
C     appropriate processing routine (or, for simple commands, performs the
C     processing directly).
C
C     Inputs:
C        Card   - Command line from input data file [Character*80]
C        Tokens - Parsed results from input line provided to the
C                 parser [Character*80 Array]
C
C     Outputs:
C        Execute_Run - Indicates that a run is to be executed
C                      [Logical]
C
C     Internal variables and constants:
C        Command - Uppercase copy of first token [Character*80]
C        Exit    - Flag used to indicate exit from routine [Logical]
C        I       - Loop index [Integer]
C
C     Include global commons
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'parser.inc'
C
C     Exceptions to default type
C
      External UpCase
      Integer I
      Logical Execute_Run,
     &        Exit
      Character*1 UpCase
      Character*80 Card,
     &             Command
C
C     Commands & command formats:
C
C       PRINT - Prints selected variable
C         PRINT {Variable) {Location)
C       RUN   - Runs the simulation and returns for more input
C         RUN
C       SET   - Assigns a value to a variable prior to initialization
C         SET (Variable) = (Value) (Location)
C       STOP  - Stops the driver
C         STOP
C
C     Begin loop to read in cards
C
      Execute_Run = .FALSE.
      Exit        = .FALSE.
      Do While (.not. Exit)
C
C     Read in next card and parse card into tokens
C
         Read(99, '(A80)', END=100) Card
         Go To 101
 100     Execute_Run = .False.
         Go To 999
 101     Call Parse_Card(Card)
         Do I = 1, 5
            Command(I:I) = UpCase(Tokens(1)(I:I))
         End Do
C
C     Call decoding routine for this card
C
         If (Command(1:5) .eq. 'PRINT') Then
            Call Print_Card
         Else If (Command(1:3) .eq. 'RUN')    Then
            Execute_Run = .TRUE.
            Exit        = .TRUE.
         Else If (Command(1:3) .eq. 'SET') Then
            Call Set_Card
         Else If (Command(1:4) .eq. 'STOP') Then
            Exit = .TRUE.
C
C     Comment card, take no action
C
         Else If (Command(1:1) .eq. ' ') Then
            Continue
C
C     Card matches none of the above, error condition
C
         Else
            Print *, ' Unreadable input card'
         End If
      End Do
C
C     Exit to driver
C
 999  Continue
C
      Return
      End
