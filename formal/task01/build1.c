// This program simulates a mass-spring-damper system.
#include <stdio.h>
#include "state.h"

int main(void) {
    //  System parameters
    double dt = 0.01;
    double tstop = 2.50;

    // Set initial conditions
    State state = {.item = {0.0, 0.0, 0.0}};

    // Run the simulation, saving the list of states.
    StateList *states = run_sim(state, tstop, dt);

    // Print results to file and console.
    char filename[50] = "output.dat";
    FILE *f = fopen(filename, "w");
    if (!f) exit(2);
    for (StateList* cell = states; cell; cell = cell->next) {
        double time = cell->data.item[TimeItem];
        double x = cell->data.item[PositionItem];
        double xd = cell->data.item[VelocityItem];
        printf("%3.15e\t%3.15e\t%3.15e\t\n", time, x, xd);
        fprintf(f, "%3.15e\t%3.15e\t%3.15e\t\n", time, x, xd);
    }
    fclose(f);
    return EXIT_SUCCESS;
}
