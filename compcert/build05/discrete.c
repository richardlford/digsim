#include "discrete.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/event.h"

#define coef_of_restitution real_array[10].r  // Coefficent of restitution [Real]
#define xd                  real_array[15].r  // Velocity of ball [m/sec]
#define BOUNCE 10
void discrete() {
  // This subroutine processes the scheduled user-defined discrete events.
  // Outputs:
  //     ReEval_Derivs   - Indicates states changed value at a discrete event [Logical]
  //     Xd              - Velocity of ball [m/sec]
  // Check for valid user event
  if (BOUNCE == events[0])
    xd = -coef_of_restitution*xd;
  reeval_derivs = TRUE;
}
