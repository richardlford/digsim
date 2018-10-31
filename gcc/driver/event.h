#ifndef EVENT
#define EVENT

#include "../driver/digtypes.h"

// Event common variables and declaration
#define MAX_NUMBER_OF_EVENTS 100
extern INT8 events[];
extern INT8 number_of_events;
extern REAL time_of_events[];

// Event parameter definitions
extern const INT8 LOG_PRINT_DATA;
extern const INT8 TERMINATE_SIMULATION;

#endif

