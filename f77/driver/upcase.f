C
      Character*1 Function UpCase(InChr)
C
C     This subroutine returns the uppercase representation of InChar.
C
C     Inputs:
C        InChar - Input character (Character*l]
C
C     Outputs:
C        UpCase - Uppercase representation of InChar [Character*l]
C
C     Internal variables and constants:
C        None.
C
      Implicit None
      Character*1 InChr
C
      If ((Ichar(InChr) .ge. 97) .and. (Ichar(InChr) .le. 122)) Then
         UpCase = Char(Ichar(InChr) - 32)
      Else
         UpCase = InChr
      End If
C
      Return
      End
