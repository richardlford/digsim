#ifndef STATE_DIGSIM01_H
#define STATE_DIGSIM01_H

enum state_size_enum
  {
   StateSize = 3
  };

typedef struct s_state {
  double StateVector[StateSize];
} State;

#endif //STATE_DIGSIM01_H
