#include <stdio.h>
#include "global.h"
#include "sysvars.h"
void differential_equations() {
  #define damping_coefficient real_array[10].r  // Damping force per velo'city [N/m/s]
  #define gravity             real_array[11].r  // Acceleration due to gravity [m/sec**2]
  #define mass                real_array[12].r  // Restoring force per position [N/m]
  #define spring_coefficient  real_array[13].r  // Mass suspended from spring [Kg]
  #define x                   real_array[16].r  // Position of suspended mass (m]
  #define xd                  real_array[17].r  // Velocity of suspended mass [m/sec]
  #define xdd                 real_array[18].r  // Acceleration of suspended mass [m/sec**2]
  // Print states for this time
  printf("%.4f\t%.6f\t%.6f\n",time, x, xd);
  // Calculate derivative at current time
  xdd = -(spring_coefficient*x+damping_coefficient*xd)/mass - gravity;
}
