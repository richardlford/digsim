#include "actuator.h"
#include "../driver/global.h"
#include "../driver/defstate.h"
#include "../driver/limit.h"

#define rdtodg         real_array[51].r
#define fin_1_cmd      real_array[300].r
#define fin_2_cmd      real_array[301].r
#define fin_3_cmd      real_array[302].r
#define fin_4_cmd      real_array[303].r
#define fin_1_position real_array[800].r
#define fin_2_position real_array[801].r
#define fin_3_position real_array[802].r
#define fin_4_position real_array[803].r
#define fin_limit_dg   real_array[804].r
#define fin_limit      real_array[805].r

void actuator_data() {
  // Fin actuators model default data
  //
  // Inputs:
  //   None.
  //
  // Outputs:
  //   Fin_Limit_dg - Maximum allowable fin deflection [deg]
  // Begin default data definition:
  fin_limit_dg = 20.0;
}

void actuator_init(void) {
  // Fin actuators model initialization
  //
  // Inputs:
  //   Fin_Limit_dg - Maximum allowable fin deflection [deg]
  //
  // Outputs:
  //   Fin_Limit - Maximum allowable fin deflection [rad]
  //
  // Internal variables and constants:
  //   RDTODG - Radians to degrees conversion factor [deg/rad]
  // Begin math model initialization:
  // Convert from input units to simulation units
  fin_limit = fin_limit_dg/rdtodg;
}

void actuator() {
  // This model simulates four perfect fin actuators, with position limits.
  //
  // Inputs:
  //   Fin_1_Cmd - Fin position commands [rad]
  //   Fin_2_Cmd
  //   Fin_3_Cmd
  //   Fin_4_Cmd
  //   Fin_Limit - Maximum allowable fin deflection [rad]
  //
  // Outputs:
  //   Fin_1_Position - Fin positions [rad]
  //   Fin_2_Position
  //   Fin_3_Position
  //   Fin_4_Position
  // Perfect actuator response to position commands
  fin_1_position = limit(fin_1_cmd, -fin_limit, fin_limit);
  fin_2_position = limit(fin_2_cmd, -fin_limit, fin_limit);
  fin_3_position = limit(fin_3_cmd, -fin_limit, fin_limit);
  fin_4_position = limit(fin_4_cmd, -fin_limit, fin_limit);
}
