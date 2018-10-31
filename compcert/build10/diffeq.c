#include "diffeq.h"
#include "actuator.h"
#include "aero.h"
#include "airframe.h"
#include "fcomp.h"
#include "gyro.h"
#include "kmtc.h"
#include "mass.h"
#include "seeker.h"
#include "target.h"
void differential_equations() {
  // This routine calls the routines that evaluate the state derivatives
  actuator();
  aerodynamics();
  mass_data();
  kinematics();
  target();
  seeker();
  gyro();
  flight_computer();
}
