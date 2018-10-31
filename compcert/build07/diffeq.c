#include "diffeq.h"
#include "kmtc.h"
#include "seeker.h"
#include "fcomp.h"
#include "airframe.h"
void differential_equations() {
  // This routine calls the modules that evaluate the state derivatives for the
  // terminal homing simulation.
  kinematics();
  seeker();
  flight_computer();
  airframe_response();
}
