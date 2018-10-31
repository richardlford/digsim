#include "../driver/setval.h"

// Current number of real variables to be initialized [Integer]
INT8 number_of_real_values;

// Real_Array locations for initial values [Integer Array]
INT8 set_real_value_index[MAX_NUMBER_OF_REAL_VALUES];

// Initial values for real variables [Real Array]
CELL set_real_value[MAX_NUMBER_OF_REAL_VALUES];
