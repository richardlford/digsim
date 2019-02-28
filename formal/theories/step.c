// A step in a Euler solver
#include "state.h"

void step(State* stp, State (*xddp)(State*), double dt) {
  State derivatives = xddp(stp);
  for(int i=0; i < StateSize; i++) {
    stp->StateVector[i] += derivatives.StateVector[i] * dt;
  }
}
