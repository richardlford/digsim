#include "diffeq.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/schedule.h"
#include <math.h>
// Inputs
#define velocity      real_array[13].r // Velocity of missile [m/sec]
#define guidance_gain real_array[25].r // Commanded angular rate per measured LOS rate [Real]
// Outputs
#define x             real_array[16].r // X position of missile [m]
#define z             real_array[17].r // Z position of missile [m]
#define theta         real_array[18].r // Attitude of missile [rad]
#define xd            real_array[19].r // X velocity of missile [m/sec]
#define zd            real_array[20].r // Z velocity of missile [m/sec]
#define thetadot      real_array[21].r // Attitude rate missile [rad/sec]
#define thetadot_cmd  real_array[22].r // Commanded attitude rate of missile [rad/sec]
#define q_s           real_array[23].r // LOS rate [rad/sec]
#define q_s_meas      real_array[24].r // Measured LOS rate {rad/sec]

void differential_equations(void) {
  // Begin math model
  // Calculate velocities
  xd =  velocity*cos(theta);
  zd = -velocity*sin(theta);
  // Calculate true LOS rate
  q_s = (z*xd-x*zd)/(x*x+z*z);
  // Calculate measured LOS rate
  q_s_meas = q_s;
  // Calculate guidance comand
  thetadot_cmd = guidance_gain*q_s_meas;
  // Calculate airframe rate
  thetadot = thetadot_cmd;
}
