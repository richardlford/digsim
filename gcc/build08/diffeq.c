#include "diffeq.h"
#include "airframe.h"
#include "fcomp.h"
#include "kmtc.h"
#include "seeker.h"
#include "target.h"
void differential_equations() {
  // This routine calls the modules that evaluate the state derivatives for the
  // terminal homing simulation.
  airframe_response();
  target();
  seeker();
  flight_computer();
  kinematics();
}
