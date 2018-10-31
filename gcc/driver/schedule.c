#include "../driver/schedule.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/event.h"
void schedule(REAL time_of_event, INT8 event) {
  // This subroutine schedules the desired events in the queue.
  //
  // Inputs:
  //    Number_Of_Events  - Current number of events in the event queue [Integer]
  //    Event             - Variable representing a discrete event [Integer]
  //    Time_Of_Event     - Time to execute requested event [Real Array]
  //
  // Outputs:
  //    Number_Of_Events  - Current number of events in the event queue [Integer]
  //    Events            - Event queue [Integer Array]
  //    Time_Of_Events    - Time to execute corresponding event in event queue [Real Array]
  //
  // Internal variables and constants:
  //    I                       - Temporary storage variable for previous number of events [Integer]
  //    MAX_NUMBER_OF_EVENTS    - Maximum allowable number of events in the event queue [Integer]
  //
  int i;
  // Check for an empty event queue; if empty, enter event in first slot
  if (number_of_events <= 0) {
    time_of_events[0] = time_of_event;
    events[0] = event;
    number_of_events = 1;
  }
  // If event queue is not empty and queue is not full, insert the event such
  // that the events are in chronological order.
  else if (number_of_events <= MAX_NUMBER_OF_EVENTS) {
    i = number_of_events;
    number_of_events = number_of_events + 1;
    while ((time_of_event < time_of_events[i-1]) && (i >= 1)) {
      time_of_events[i] = time_of_events[i-1];
      events[i] = events[i-1];
      i -= 1;
    }
    time_of_events[i] = time_of_event;
    events[i] = event;
  }
  else
    printf(" Too many events scheduled\n");
}
