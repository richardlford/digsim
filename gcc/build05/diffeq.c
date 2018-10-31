#include "diffeq.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/schedule.h"

#include <math.h>

#define gravity real_array[11].r  // Acceleration due to gravity [m/sec**2]
#define x       real_array[14].r  // Position of ball [m]
#define xd      real_array[15].r  // Velocity of ball [m/sec]
#define xdd     real_array[16].r  // Acceleration of ball [m/sec**2]

#define BOUNCE 10  // ball hits floor event

void differential_equations(void) {
  REAL dtimpact; // Time until impact [Sec]
  // Calculate acceleration at current time
  xdd = -gravity;
  // Check to see if impact will occur within a maximum time step
  if ((x + xd*dtmax + 0.5*xdd*dtmax*dtmax) <= 0.0) {
    // If so, check to see if it is too late
    if ((x+xd*dtmin + 0.5*xdd*dtmin*dtmin) <= 0.0) { // If too late, set delay until impact to zero
      dtimpact = 0.0;
    }
    else { // Otherwise, calculate delay until actual impact
      dtimpact = (-xd - sqrt(xd*xd - 2.0*x*xdd))/(2.0*x);
      if (dtimpact < dtmin)
        dtimpact = (-xd + sqrt(xd*xd-2.0*x*xdd))/(2.0*x);
    }
    // Schedule impact
    schedule((time+dtimpact),BOUNCE);
  }
}
