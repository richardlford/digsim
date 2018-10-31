#include "diffeq.h"
#include "airframe.h"
#include "fcomp.h"
#include "gyro.h"
#include "kmtc.h"
#include "seeker.h"
#include "target.h"
void differential_equations() {
  // This routine calls the routines that evaluate the state derivatives
  airframe_response();
  kinematics();
  target();
  seeker();
  gyro();
  flight_computer();
}
