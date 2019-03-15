#include "state.h"

// Return first derivative of state components
void diffeq(State* state, State* derivatives_out) {
  double damping_coefficient =  8.88;
  double gravity             =  9.88;
  double mass                =  1.00;
  double spring_coefficient  = 39.47;

  // Unpack state into locals.
  double time = state->item[TimeItem];
  double x = state->item[PositionItem];
  double xd = state->item[VelocityItem];

  State result;
  // dt/dt = 1.0
  derivatives_out->item[TimeItem] = 1.0;
  // dx/dt = velocity
  derivatives_out->item[PositionItem] = xd;
  // acceleration = dv/dt
  derivatives_out->item[VelocityItem] = -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;
}

