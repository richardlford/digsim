#include "term.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include <stdio.h>
#include <math.h>


#define x_bi_i  real_array[500].r // X position of BCS w.r.t. ICS, expressed in the ICS [m]
#define z_bi_i  real_array[501].r // Z position of BCS w.r.t. ICS, expressed in the ICS [m]
#define xd_bi_i real_array[502].r // X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
#define zd_bi_i real_array[503].r // Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]

BOOL termination_conditions(BOOL quit) {
  // Termination module
  // Inputs:
  //   Z_bi_i - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
  // Outputs:
  //   Quit - Stop simulation run [Boolean]
  if (z_bi_i >= 0.0) {
    miss();
    quit = TRUE;
  }
}

void miss() {
  // Miss distance calculation routine
  //
  // Inputs:
  //   X_bi_i  - X position of BCS w.r.t. ICS, expressed in the ICS [m]
  //   Xd_bi_i - X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //   Z_bi_i  - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
  //   Zd_bi_i - Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  //
  // Outputs:
  //   DtMiss - Time step to closest approach [sec]
  //   RMiss  - Missile miss distance [m]
  REAL dtmiss,rmiss;
  FILE *missdat;
  // Begin miss distance calculation:
  // Evaluate closest approach time and miss distance
  dtmiss = (x_bi_i*xd_bi_i + z_bi_i*zd_bi_i)/(pow(xd_bi_i,2) + pow(zd_bi_i,2));
  rmiss = sqrt( pow(x_bi_i - xd_bi_i*dtmiss,2) + pow(z_bi_i-zd_bi_i*dtmiss,2));
  missdat = fopen("miss.dat", "w");
  fprintf(missdat," RMiss = %f", rmiss);
  fprintf(missdat," DtMiss = %f", dtmiss);
  fclose(missdat);
}
