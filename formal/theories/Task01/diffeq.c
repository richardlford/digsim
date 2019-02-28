#include "state.h"

// Return first derivative of state components
State diffeq(State state) {
  double damping_coefficient =  8.88;
  double gravity             =  9.88;
  double mass                =  1.00;
  double spring_coefficient  = 39.47;

  // Unpack state into locals.
  double time = state.item[TimeItem];
  double x = state.item[PositionItem];
  double xd = state.item[VelocityItem];
  
  State result;
  // dt/dt = 1.0
  result.item[TimeItem] = 1.0;
  // dx/dt = velocity
  result.item[PositionItem] = xd;
  // acceleration = dv/dt
  result.item[VelocityItem] = -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;
  return result;
}

