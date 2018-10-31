#include "../driver/global.h"
#include "../driver/sysvars.h"
void driver_default_data() {
  // This subroutine sets the default values for the user-accessable
  // system variables.
  // Set system default values
  time    = 0.0;
  time0   = 0.0;
  tstop   = 0.0;
  dt      = 0.005;
  dtmax   = 0.005; // Maximum allowable simulation time step [sec]
  dtmin   = 0.005; // Minimum allowable simulation time step [sec]
  dtprint = 0.01;  // Time step between printing data [sec]
  reeval_derivs = FALSE;
}
