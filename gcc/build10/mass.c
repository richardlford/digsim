#include "mass.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"

#define ixx_b  real_array[400].r
#define iyy_b  real_array[401].r
#define izz_b  real_array[402].r
#define mass_b real_array[403].r

void mass_data() {
  // This subroutine sets the default data for the missile mass characteristics.
  // Inputs:
  //   None.
  // Outputs:
  //   Ixx_B  - Diagonal elements of the inertia tensor for the missile [Kg*m**2]
  //   Iyy_B
  //   Izz_B
  //   Mass_B - Missile mass [Kg]
  // Begin default data definition:
  ixx_b = 0.10;
  iyy_b = 0.75;
  izz_b = 0.75;
  mass_b = 10.0;
}

void mass_init() {
  // This subroutine initializes the variables the missile mass characteristics.
}
void mass() {
  // This subroutine processes the dynamic missile mass characteristics.
}
