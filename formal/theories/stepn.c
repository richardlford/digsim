// A step in a Euler solver
void stepn(double* state, double* diffs, unsigned n, double dt) {
  for (int i = 0; i < n; i++) {
    state[i] += diffs[i] * dt;
  }
}
