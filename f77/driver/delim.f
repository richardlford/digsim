C
      Logical Function Match_Delimiter(InChar)
C
C     Inputs:
C        InChar - Character to test against stored delimiters [Character*l]
C
C     Outputs:
C        Match-Delimiter      - Indicates input character is one of the stored
C                               delimiters [Logical]
C
C     Internal variables and constants:
C        Delimiters           - Array of characters used to separate tokens
C                               [Character*l Array]
C        Match                - Intermediate storage variable for results of
C                               test [Logical]
C
C        Number Of Delimiters - Number of token delimiters to test [Integer]
C        I                    - Loop index [Integer]
C
      Implicit None
C
      Character*1 Delimiters(4),
     &            InChar
      Integer I,
     &        Number_Of_Delimiters
      Logical Match
C
C     Initialize delimiters
C
      Data Number_Of_Delimiters /4/
      Data Delimiters /' ', '=', ';', ':'/
C
      I     = 1
      Match =  .FALSE.
      Do While ((.not. Match) .and. (I .le. Number_Of_Delimiters))
         Match = InChar .eq. Delimiters(I)
         I     = I + 1
      End Do
      Match_Delimiter = Match
C
      Return
      End
