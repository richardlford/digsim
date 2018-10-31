#include "term.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include <stdio.h>
#include <math.h>

#define x_tb_i  real_array[606].r
#define xd_tb_i real_array[607].r
#define y_tb_i  real_array[608].r
#define yd_tb_i real_array[609].r
#define z_tb_i  real_array[610].r
#define zd_tb_i real_array[611].r

BOOL termination_conditions(BOOL quit) {
  // This module determines if the run termination conditions have been met.
  //
  // Inputs:
  //   X_tb_i  - Position of the target w.r.t. the BCS, expressed in the ICS [m]
  //   Y_tb_i
  //   Z_tb_i
  //   Xd_tb_i - Velocity of the target w.r.t. the BCS, expressed in the ICS
  //   Yd_tb_i   [m/sec]
  //   Zd_tb_i
  //
  // Outputs:
  //   Quit - Stop simulation run [Boolean]
  //
  // Internal variables and constants:
  //   Time_To_Go - Time until closest approach [sec]
  REAL time_to_go;
  // Begin consition check:
  // Evaluate closest approach time and miss distance
  time_to_go = -(x_tb_i*xd_tb_i + y_tb_i*yd_tb_i + z_tb_i*zd_tb_i)/(pow(xd_tb_i,2) + pow(yd_tb_i,2) + pow(zd_tb_i,2));
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
  //   X_tb_i  - Position of the target w.r.t. the BCS, expressed n the ICS [m]
  //   Y_tb_i
  //   Z_tb_i
  //   Xd_tb_i - Velocity of the target w.r.t. the BCS, expressed in the ICS
  //   Yd_tb_i   [m/sec]
  //   Zd_tb_i
  //
  // Outputs:
  //   DtMiss - Time step to closest approach [sec]
  //   RMiss  - Missile miss distance [m]
  REAL dtmiss,rmiss;
  FILE *missdat;
  // Begin miss distance calculation
  // Evaluate closest approach time and miss distance
  dtmiss = -(x_tb_i*xd_tb_i+y_tb_i*yd_tb_i+z_tb_i*zd_tb_i)/(pow(xd_tb_i,2) + pow(yd_tb_i,2) + pow(zd_tb_i,2));
  rmiss = sqrt(pow(x_tb_i - xd_tb_i*dtmiss,2) + pow(y_tb_i - yd_tb_i*dtmiss,2) + pow(z_tb_i - zd_tb_i*dtmiss,2));
  missdat = fopen("miss.dat", "w");
  fprintf(missdat," RMiss = %f", rmiss);
  fprintf(missdat," DtMiss = %f", dtmiss);
  fclose(missdat);
}
