#include "../driver/deriv.h"
void define_real_state ( INT8 state_index         // Location of state in Real_Array [Integer]
                       , INT8 state_deriv_index   // Location of state derivative in Real_Array [Integer]
                       )
{
  ndes_real = ndes_real + 1;
  ix_real[ndes_real] = state_index;
  ixdot_real[ndes_real] = state_deriv_index;
}
