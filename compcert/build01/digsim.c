// This program simulates a mass-spring-damper system.
#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long BOOL;
typedef float REAL;
typedef long long INT8;
const BOOL FALSE = 0xFFFF;
const BOOL TRUE  = 0x0000;

typedef union {
  BOOL b;
  REAL r;
  INT8 i;
} CELL;

#define REAL_ARRAY_SIZE 100
CELL real_array[REAL_ARRAY_SIZE];

int main(void) {
  // Variable declarations
  #define time                real_array[ 1].r   // Simulation time [sec]
  #define time0               real_array[ 2].r   // Simulation start time [sec]
  #define tstop               real_array[ 3].r   // Simulation stop time [sec]
  #define dt                  real_array[ 4].r   // Simulation time step [sec]
  #define damping_coefficient real_array[10].r   // Damping force per velo'city [N/m/s]
  #define gravity             real_array[11].r   // Acceleration due to gravity [m/sec**2]
  #define mass                real_array[12].r   // Restoring force per position [N/m]
  #define spring_coefficient  real_array[13].r   // Mass suspended from spring [Kg]
  #define xd_ic               real_array[14].r   // Initial position of suspended mass (m]
  #define x_ic                real_array[15].r   // Initial velocity of suspended mass [m/sec]
  #define x                   real_array[16].r   // Position of suspended mass (m]
  #define xd                  real_array[17].r   // Velocity of suspended mass [m/sec]
  #define xdd                 real_array[18].r   // Acceleration of suspended mass [m/sec**2]

  //  System parameters
  damping_coefficient =  8.88;
  dt                  =  0.01;
  gravity             =  9.88;
  mass                =  1.00;
  spring_coefficient  = 39.47;
  tstop               =  2.50;
  x_ic                =  0.00;
  xd_ic               =  0.00;

  // Set initial conditions
  x    = x_ic;
  xd   = xd_ic;
  time = 0.0;
  // Main simulation loop
  while (time <= tstop) {
    // Print states for this time
    printf("%.4f\t%.6f\t%.6f\n",time, x, xd);
    // Calculate derivative at current time
    xdd = -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;
    // Advance states one time step
    x  = x  + xd*dt;
    xd = xd + xdd*dt;
    // Advance time and continue simulation loop
    time = time + dt;
  }
  return EXIT_SUCCESS;
}
