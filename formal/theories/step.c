// A step in a Euler solver
#include "state.h"

void step(State* stp, double (*xddp)(double, double, double), double dt) {
  double xdd = xddp(stp->t, stp->x, stp->xd);
  stp->t += dt;
  stp->x += stp->xd * dt;
  stp->xd += xdd * dt;
}
