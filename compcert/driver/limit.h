#ifndef LIMIT
#define LIMIT
#include "../driver/digtypes.h"
extern REAL limit(REAL,REAL,REAL);
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif
