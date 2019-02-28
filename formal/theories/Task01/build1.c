// This program simulates a mass-spring-damper system.
#include <stdio.h>
#include <stdlib.h>
#include "state.h"

int main(void) {
  // Variable declarations

  //  System parameters
  double dt                  =  0.01;
  double tstop               =  2.50;

  // Set initial conditions
  State state = { { 0.0, 0.0, 0.0 } };

  // Unpack state into locals.
  double time = state.item[TimeItem];
  double x = state.item[PositionItem];
  double xd = state.item[VelocityItem];

  // Main simulation loop
  while (time <= tstop) {
    // Print states for this time
    printf("%.4f\t%.6f\t%.6f\n",time, x, xd);

    // Calculate derivative from current state.
    State derivatives = diffeq(state);
    
    // Advance state one time step
    one_step(&state, &derivatives, dt);
    time = state.item[TimeItem];
    x = state.item[PositionItem];
    xd = state.item[VelocityItem];

    // Update unpacked locals.
  }
  return EXIT_SUCCESS;
}
