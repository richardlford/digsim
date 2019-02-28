#include "state.h"

// Update state given derivatives and time delta.
void one_step(State* state, State* derivative, double dt) {
  for (int i=0; i < StateSize; i++) {
    state->item[i] += derivative->item[i] * dt;
  }
}
