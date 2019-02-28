#ifndef STATE_DIGSIM01_H
#define STATE_DIGSIM01_H

typedef enum state_items_enum
  {
   TimeItem,
   PositionItem,
   VelocityItem,
   StateSize
  } StateItem;

typedef struct s_state {
  double item[StateSize];
} State;

// Return first derivative of state components
State diffeq(State state);

// Update state given derivatives and time delta.
void one_step(State* state, State* derivative, double dt);

#endif //STATE_DIGSIM01_H
