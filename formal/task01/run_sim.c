#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "state.h"

StateList* run_sim(State state, double tstop, double dt) {

    // Get list to accumulate results.
    StateList* states = NULL;

    // Main simulation loop
    int i = 0;
    while (state.item[TimeItem] <= tstop) {
        // Record state.
        states = state_list_cons(states, state);

        // Calculate derivative from current state.
        State derivatives = diffeq(state);

        // Advance state one time step
        one_step(&state, &derivatives, dt);

        // Kludge to get time showing up nicely.
        i++;
        state.item[TimeItem] = round(i * dt * 1.0e6) / 1.0e6;
    }
    states = reverse_state_list(states);
    return states;
}
