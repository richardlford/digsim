#include "term.h"
#include <stdio.h>
#include <math.h>
#include "../driver/global.h"

#define x  real_array[16].r // X position of missile [m]
#define z  real_array[17].r // Z position of missile [m]
#define xd real_array[19].r // X velocity of missile [m/sec]
#define zd real_array[20].r // Z velocity of missile [m/sec]

BOOL termination_conditions(BOOL quit) {
  // This module determines if the run termination conditions have been met
  if (z >= 0.0) {
    miss();
    quit = TRUE;
  }
  return quit;
}

void miss() {
  // Miss distance calculation routine
  // Outputs:
  //   DtMiss - Time step to closest approach [sec]
  //   RMiss  - Missile miss distance [m]
  REAL dtmiss,rmiss;
  FILE *missdat;
  // Evaluate closest approach time and miss distance
  dtmiss = (x*xd + z*zd)/(pow(xd,2) + pow(zd,2));
  rmiss  = sqrt(pow((x - xd*dtmiss),2) + pow((z-zd*dtmiss),2));
  missdat = fopen("miss.dat", "w");
  fprintf(missdat," RMiss = %f", rmiss);
  fprintf(missdat," DtMiss = %f", dtmiss);
  fclose(missdat);
}
