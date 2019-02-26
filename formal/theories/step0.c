// A step in a Euler solver
double step0(double x, double (*xdp)(double)) {
  double xd = xdp(x);
  double result = x + xd;
  return result;
}
