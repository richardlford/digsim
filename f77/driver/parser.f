C
      Subroutine Parse_Card(Card)
C
C     This subroutine pasres the 80 character string in card according
C     to the delimiters in the delimiter array (any sequence of
C     delimiters is also considered a delimiter). Anything enclosed in
C     curly brackets is ignored, while quotes override other delimiters
C     to declare a token.
C
C     The tokens (up to MAX_NUMBER_OF_TOKENS) are returned in the token
C     array.
C
C     The tokens are passed through a common block.
C
C     Inputs:
C        Card - Input line to be seperated into tokens [Character*80]
C
C     Outputs:
C        Tokens - Parsed results from input line provided to the parser [Character*80 Array]
C
C     Internal variables and constants:
C        Card_Index           - Index of character in input line: being processed
C                               [Integer]
C        First_Char           - Index of first character of a token in the input
C                               line (Integer]
C        Last_Char            - Index of last character of a token in the input
C                               line (Integer]
C        MAX NUMBER OF TOKENS - Maximum allowable number of parsed tokens [Integer)
C        Token                - Count of number of tokens extracted from this
C                               command line [Integer]
C
      Implicit None
      Include 'parser.inc'
C
C     Declare variables
C
      External Match_Delimiter
      Character*80 Card
      Logical Match_Delimiter
      Integer I, Card_Index, Token,
     &        First_Char, Last_Char
C
C     Initialize delimiters, comment characters, and current token
C
      Token = 0
      Do I = 1, MAX_NUMBER_OF_TOKENS
         Write(Tokens(I), '(80X)')
      End Do
C
C     Loop through characters in card looking for delimiters
C
      Card_Index = 1
      Do While ((Card_Index .le. 80) .and.
     &          (Token .le. MAX_NUMBER_OF_TOKENS))
C
C     Check for comment section; if found look for closing brace
C
         If (Card(Card_Index:Card_Index) .eq. '{') Then
            Last_Char = Card_Index +
     &                  Index(Card((Card_Index + 1):80), '}')
            If (Last_Char .eq. Card_Index) Then
            Card_Index = 81
         Else
            Card_Index = Last_Char + 1
         End If
C
C     Check for quoted section; if found look for closing quote (' or ")
C     & write token
C
      Else If ((Card(Card_Index:Card_Index) .eq. Char(39)) .or.
     &        (Card(Card_Index:Card_Index)  .eq. Char(34))) Then
         First_Char = Card_Index + 1
         If (Card(Card_Index:Card_Index) .eq. Char(39)) Then
            Last_Char = Card_Index +
     &                  Index(Card(First_Char:80), Char(39)) - 1
         Else
            Last_Char = Card_Index +
     &                  Index(Card(First_Char:80), Char(34)) - 1
         End If
         If (Last_Char .eq. (Card_Index- 1)) Then
            Last_Char = 80
         End If
         Token = Token + 1
         Tokens(Token)(1:(Last_Char - First_Char + 1)) =
     &     Card(First_Char:Last_Char)
         Card_Index = Last_Char + 2
C
C     Check for delimiter; if found check for multiple delimeters
C
      Else If (Match_Delimiter(Card(Card_Index:Card_Index))) Then
         Do While (Match_Delimiter(Card(Card_Index:Card_Index)))
            Card_Index =  Card_Index + 1
         End Do
         First_Char = Card_Index
C
C     Check for end of token and save
C
         Do While
     &     ((.not. Match_Delimiter(Card(Card_Index:Card_Index)))
     &       .and. (Card(Card_Index:Card_Index) .ne. '{')
     &       .and. ((Card(Card_Index:Card_Index) .ne. Char(39)) .or.
     &              (Card(Card_Index:Card_Index) .ne. Char(34)))
     &        .and. (Card_Index .le. 80))
            Card_Index = Card_Index + 1
         End Do
         Last_Char = Card_Index - 1
         Token     = Token + 1
         Tokens(Token)(1:(Last_Char - First_Char + 1)) =
     &     Card(First_Char:Last_Char)
C
C     Not a comment, quote or delimiter so must be a token. Find end of
C     token & write
c
      Else
         First_Char = Card_Index
         Do While
     &     ((.not. Match_Delimiter(Card(Card_Index:Card_Index)))
     &       .and. (Card(Card_Index:Card_Index) .ne. '(')
     &       .and. ((Card(Card_Index:Card_Index) .ne. Char(39)) .or.
     &              (Card(Card_Index:Card_Index) .ne. Char(34)))
     &        .and. (Card_Index .le. 80))
            Card_Index = Card_Index + 1
         End Do
         Last_Char = Card_Index - 1
         Token     = Token + 1
         Tokens(Token)(1:(Last_Char - First_Char + 1)) =
     &     Card(First_Char:Last_Char)
C
C     End of token loop
C
      End If
C
C     Get rid of trailing delimiters
C
      Do While ((Match_Delimiter(Card(Card_Index:Card_Index)))
     &          .and. (Card_Index .le. 80))
         Card_Index = Card_Index + 1
      End Do
C
C     End of card parsing section
C
      End Do
C
      Return
C
      End
