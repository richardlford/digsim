#include "term.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include <stdio.h>
#include <math.h>

#define x_bt_i  real_array[602].r
#define z_bt_i  real_array[603].r
#define xd_bt_i real_array[604].r
#define zd_bt_i real_array[605].r

BOOL termination_conditions(BOOL quit) {
  // This module determines if the run termination conditions have been met
  //
  // Inputs:
  //     X_bt_i  - X position of the BCS w.r.t. the target, expressed in the ICS [m]
  //     Xd_bt_i - X velocity of the BCS w.r.t. the target, expressed in the ICS
  //                       [m/sec]
  //     Z_bt_i  - Z position of the BCS w.r.t. the target, expressed in the ICS [m]
  //     Zd_bt_i - Z velocity of the BCS w.r.t. the target, expressed in the ICS [m/sec]
  //
  // Outputs:
  //     Quit - Stop simulation run [Boolean]
  //
  // Internal variables and constants:
  //     Time_To_Go - Time until closest approach [sec]
  //
  // Declare global common and assign variable locations
  // Begin consition check:
  // Evaluate closest approach time and miss distance
  REAL time_to_go;
  time_to_go = -(x_bt_i*xd_bt_i + z_bt_i*zd_bt_i)/(pow(xd_bt_i,2) + pow(zd_bt_i,2));
  if (time_to_go < 0.0) {
    miss();
    quit = TRUE;
  }
  return quit;
}

void miss() {
  // Miss distance calculation routine
  //
  // Inputs:
  //     X_bt_i  - X position of the BCS w.r.t. the target, expressed in the ICS [m]
  //     Xd_bt_i - X velocity of the BCS w.r.t. the target, expressed in the ICS [m/sec]
  //     Z_bt_i  - Z position of the BCS w.r.t. the target, expressed in the ICS [m]
  //     Zd_bt_i - Z velocity of the BCS w.r.t. the target, expressed in the ICS [m/sec]
  //
  // Outputs:
  //     DtMiss  - Time step to closest approach [sec]
  //     RMiss   - Missile miss distance [m]
  REAL dtmiss,rmiss;
  FILE *missdat;
  // Begin miss distance calculation:
  // Evaluate closest approach time and miss distance
  dtmiss = (x_bt_i*xd_bt_i + z_bt_i*zd_bt_i)/(pow(xd_bt_i,2) + pow(zd_bt_i,2));
  rmiss  = sqrt(pow(x_bt_i - xd_bt_i*dtmiss,2) + pow(z_bt_i - zd_bt_i*dtmiss,2));
  missdat = fopen("miss.dat", "w");
  fprintf(missdat," RMiss = %f", rmiss);
  fprintf(missdat," DtMiss = %f", dtmiss);
  fclose(missdat);
}
