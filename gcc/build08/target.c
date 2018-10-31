#include "target.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"

#define x_ti_i_ic  real_array[704].r
#define xd_ti_i_ic real_array[705].r
#define z_ti_i_ic  real_array[706].r
#define zd_ti_i_ic real_array[707].r
#define x_ti_i     real_array[700].r
#define xd_ti_i    real_array[701].r
#define z_ti_i     real_array[702].r
#define zd_ti_i    real_array[703].r

void target_data() {
  // This subroutine sets the default data for the target motion
  // Inputs:
  //   None.
  // Outputs:
  //   X_ti_i_IC  - Initial X position of target w.r.t. ICS, expressed in ICS [m]
  //   Xd_ti_i_IC - Initial X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //   Z_ti_i_IC  - Initial Z position of target w.r.t. ICS, expressed in ICS [m]
  //   Zd_ti_i_IC - Initial Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  // Internal variables and constants
  //   None.
  // Declare global common and assign variable locations
  // Begin default data definition:
  x_ti_i_ic  = 0.0;
  z_ti_i_ic  = 0.0;
  xd_ti_i_ic = 0.0;
  zd_ti_i_ic = 0.0;
  //
}
void target_init() {
  // This subroutine initializes the variables for the target motion model.
  //
  // Inputs:
  //   X_ti_i_IC  - Initial X position of target w.r.t. ICS, expressed in ICS [m]
  //   Xd_ti_i_IC - Initial X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //   Z_ti_i_IC  - Initial Z position of target w.r.t. ICS, expressed in ICS [m]
  //   Zd_ti_i_IC - Initial Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  // Outputs:
  //   X_ti_i  - X position of target w.r.t. ICS, expressed in ICS [m]
  //   Xd_ti_i - X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //   Z_ti_i  - Z position of target w.r.t. ICS, expressed in ICS [m]
  //   Zd_ti_i - Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //   Internal variables and constants:
  //       None.
  //   Declare global common and assign variable locations
  //
  //       Begin math model initialization:
  //
  //       Initialize target states
  //
  x_ti_i  = x_ti_i_ic;
  z_ti_i  = z_ti_i_ic;
  xd_ti_i = xd_ti_i_ic;
  zd_ti_i = zd_ti_i_ic;
}
void target() {
  // This subroutine determined the target motion.
  // Inputs:
  //     X_ti_i_IC       - Initial X position of target w.r.t. ICS, expressed in ICS [m]
  //     Z_ti_i_IC       - Initial Z position of target w.r.t. ICS, expressed in ICS [m]
  // Outputs:
  //     X_ti_i  - X position of target w.r.t. ICS, expressed in ICS [m]
  //     Xd_ti_i - X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  //     Z_ti_i  - Z position of target w.r.t. ICS, expressed in ICS [m]
  //     Zd_ti_i - Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  // Begin math model:
  // Calculate current target position
  x_ti_i = x_ti_i_ic + xd_ti_i*time;
  z_ti_i = z_ti_i_ic + zd_ti_i*time;
}
