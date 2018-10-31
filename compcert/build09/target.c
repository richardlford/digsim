#include "target.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"

#define x_ti_i     real_array[700].r
#define xd_ti_i    real_array[701].r
#define y_ti_i     real_array[702].r
#define yd_ti_i    real_array[703].r
#define z_ti_i     real_array[704].r
#define zd_ti_i    real_array[705].r
#define x_ti_i_ic  real_array[706].r
#define y_ti_i_ic  real_array[707].r
#define z_ti_i_ic  real_array[708].r
#define xd_ti_i_ic real_array[709].r
#define yd_ti_i_ic real_array[710].r
#define zd_ti_i_ic real_array[711].r

void target_data() {
  // This subroutine sets the default data for the target motion
  //
  // Inputs:
  //     None.
  //
  // Outputs:
  //     X_ti_i_IC  - Initial X position of target w.r.t. ICS, expressed in ICS [m]
  //     Xd_ti_i_IC - Initial X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //     Z_ti_i_IC  - Initial Z position of target w.r.t. ICS, expressed in ICS [m]
  //     Zd_ti_i_IC - Initial Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  // Begin default data definition:
  x_ti_i_ic = 500.0;
  y_ti_i_ic = -250.0;
  z_ti_i_ic = 0.0;
  xd_ti_i_ic = -25.0;
  yd_ti_i_ic = 25.0;
  zd_ti_i_ic = 0.0;
}
void target_init() {
  // This subroutine initializes the variables for the target motion model.
  // Inputs:
  //     X_ti_i_IC  - Initial position of target w.r.t. ICS, expressed in ICS [m]
  //     Y_ti_i_IC
  //     Z_ti_i_IC
  //     Xd_ti_i_IC - Initial velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //     Yd_ti_i_IC
  //     Zd_ti_i_IC
  // Outputs:
  //     X_ti_i  - position of target w.r.t. ICS, expressed in ICS [m]
  //     Y_ti_i
  //     Z_ti_i
  //     Xd_ti_i - velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //     Yd_ti_i
  //     Zd_ti_i
  // Begin math model initialization:
  // Initialize target states
  x_ti_i = x_ti_i_ic;
  y_ti_i = y_ti_i_ic;
  z_ti_i = z_ti_i_ic;
  xd_ti_i = xd_ti_i_ic;
  yd_ti_i = yd_ti_i_ic;
  zd_ti_i = zd_ti_i_ic;
}
void target() {
  // This subroutine determined the target motion.
  // Inputs:
  //     Time       - Simulation time [sec]
  //     X_ti_i_IC  - Initial position of target w.r.t. ICS, expressed in ICS [m]
  //     Y_ti_i_IC
  //     Z_ti_i_IC
  //     Xd_ti_i - velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //     Yd_ti_i
  //     Zd_ti_i
  // Outputs:
  //     X_ti_i  - position of target w.r.t. ICS, expressed in ICS [m]
  //     Y_ti_i
  //     Z_ti_i
  // Begin math model:
  // Calculate current target position
  x_ti_i = x_ti_i_ic + xd_ti_i*time;
  y_ti_i = y_ti_i_ic + yd_ti_i*time;
  z_ti_i = z_ti_i_ic + zd_ti_i*time;
}
