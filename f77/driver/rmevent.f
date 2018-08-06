C
      Subroutine Remove_Event
C
C     This subroutine removes the first event in the future event
C     queue. All subsequent events are then advanced in the queue.
C
C     Inputs:
C        Events            - Event queue [Integer Array]
C        Number_Of_Events  - Current number of events in the event queue [Integer]
C        Time_Of_Events    - Time to execute corresponding event in event queue [Real Array]
C
C     Outputs:
C        Events            - Event queue [Integer Array]
C        Number_Of_Events  - Current number of events in the event queue [Integer]
C        Time_Of_Events    - Time to execute corresponding event in event queue [Real Array]
C
C     Internal variables and constants:
C        I                 - Loop Index [Integer]
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'event.inc'
C
C     Exceptions to default type
C
      Integer I
C
C     Remove the first event and shift down future events list.
C     Check for valid Number_Of_Events
C
      If (Number_Of_Events .gt. 0) Then
         I = 2
         Do While (I .le. Number_Of_Events)
            Time_Of_Events(I - 1) = Time_Of_Events(I)
            Events (I - 1) = Events(I)
            I = I + 1
         End do
         Time_Of_Events(Number_of_Events) = 99999.0E0
         Events(Number_Of_Events)         = 0
         Number_Of_Events                 = Number_Of_Events - 1
C
C     Inform user if error condition exists
C
      Else
         Write (*, '(A)')
     &        'WARNING : event removal requested from empty queue.'
      End If
C
      Return
      End
