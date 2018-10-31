#include "diffeq.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#define damping_coefficient real_array[10].r  // Damping force per velocity [N/m/s]
#define gravity             real_array[11].r  // Acceleration due to gravity [m/sec**2]
#define mass                real_array[12].r  // Mass suspended from spring [Kg]
#define spring_coefficient  real_array[13].r  // Restoring force per position [N/m]
#define x                   real_array[16].r  // Position suspended mass [m]
#define xd                  real_array[17].r  // Velocity of suspended mass [m/sec]
#define xdd                 real_array[18].r  // Acceleration of suspended mass [m/sec**2]
void differential_equations(void) {
  // This routine evaluates the state derivatives for the mass-spring-damper
  // system.
  // Calculate derivative at current time
  xdd = -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;
}
