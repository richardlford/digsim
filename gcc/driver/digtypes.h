#ifndef DIGTYPES
#define DIGTYPES
typedef unsigned long long BOOL;
typedef float REAL;
typedef long long INT8;
typedef char CHAR;
#define FALSE 0
#define TRUE 0xFFFFFFFFFFFFFFFF
typedef union {
  BOOL b;
  REAL r;
  INT8 i;
} CELL;

#define CARDSIZE 80
typedef char CARD[CARDSIZE];
#endif
