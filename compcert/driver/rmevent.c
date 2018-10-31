#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/event.h"
void remove_event() {
  // This subroutine removes the first event in the future event queue. All
  // subsequent events are then advanced in the queue.
  //
  // Inputs:
  //     Events            - Event queue [Integer Array]
  //     Number_Of_Events  - Current number of events in the event queue [Integer]
  //     Time_Of_Events    - Time to execute corresponding event in event queue [Real Array]
  //
  // Outputs:
  //     Events            - Event queue [Integer Array]
  //     Number_Of_Events  - Current number of events in the event queue [Integer]
  //     Time_Of_Events    - Time to execute corresponding event in event queue [Real Array]

  // Remove the first event and shift down future events list.
  // Check for valid Number_Of_Events
  int i;
  if (number_of_events > 0) {
    i = 1;
    while (i <= number_of_events) {
      time_of_events[i-1] = time_of_events[i];
      events[i-1] = events[i];
      i += 1;
    }
    time_of_events[number_of_events] = 1.0e10;
    events[number_of_events] = 0;
    number_of_events -= 1;
    // Inform user if error condition exists
  }
  else
    printf(" WARNING : event removal requested from empty queue.\n");
}
