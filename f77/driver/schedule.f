C
      Subroutine Schedule (Time_Of_Event, Event)
C
C     This subroutine schedules the desired events in the queue.
C
C     Inputs:
C        Number_Of_Events  - Current number of events in the event queue [Integer]
C        Event             - Variable representing a discrete event [Integer]
C        Time_Of_Event     - Time to execute requested event [Real Array]
C
C     Outputs:
C        Number_Of_Events  - Current number of events in the event queue [Integer]
C        Events            - Event queue [Integer Array]
C        Time_Of_Events    - Time to execute corresponding event in event queue [Real Array]
C
C     Internal variables and constants:
C        I                       - Temporary storage variable for previous number of events [Integer]
C        MAX_NUMBER_OF_EVENTS    - Maximum allowable number of events in the event queue [Integer]
C
C     Include files
C
      Include 'global.inc'
      Include 'sysvars.inc'
      Include 'event.inc'
C
C     Exceptions to default type
C
      Integer I, Event
C
C     Check for an empty event queue; if empty, enter event in first slot
C
      If (Number_Of_Events .le. 0) Then
         Number_Of_Events  = 1
         Time_Of_Events(1) = Time_Of_Event
         Events(1)         = Event
C
C     If event queue is not empty and queue is not full, insert the event such that the events are in
C     chronological order.
C
      Else If (Number_Of_Events .lt. MAX_NUMBER_OF_EVENTS) Then
         I                = Number_Of_Events
         Number_Of_Events = Number_Of_Events + 1
         Do While
     &        ((Time_Of_Event .lt. Time_Of_Events(I)) .and. (I .ge. 1))
            Time_Of_Events(I + 1) = Time_Of_Events(I)
            Events (I + 1)        = Events(I)
            I                     = I - 1
         End Do
         Time_Of_Events(I + 1) = Time_Of_Event
         Events(I + 1)         = Event
      Else
         Write (*, '(A)' ) ' Too many events scheduled'
      End If
C
      Return
      End
