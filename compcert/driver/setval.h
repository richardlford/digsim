#ifndef SETVAL
#define SETVAL
#include "global.h"

// Maximum allowable number of real variables to be initialized [Integer]
#define MAX_NUMBER_OF_REAL_VALUES 50

extern INT8 number_of_real_values;    // Current number of real variables to be initialized [Integer]
extern INT8 set_real_value_index[];   // Real_Array locations for initial values [Integer Array]
extern CELL set_real_value[];         // Initial values for real variables [Real Array]
#endif
