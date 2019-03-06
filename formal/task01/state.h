#ifndef STATE_DIGSIM01_H
#define STATE_DIGSIM01_H

#include <stdlib.h>

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

typedef struct s_state_list_cell {
  State data;
  struct s_state_list_cell* next;
} StateListCell;

typedef struct s_state_list {
    StateListCell* first;
    StateListCell* last;
} StateList;

StateList* new_state_list(void);
StateListCell* new_state_list_cell(State data, StateListCell* next);
void state_list_append(StateList* list, State item);
  
// Return first derivative of state components
State diffeq(State state);

// Update state given derivatives and time delta.
void one_step(State* state, State* derivative, double dt);

// Starting from state, run the simulation until tstop,
// using dt as time step size.
// Returns the list of states.
StateList* run_sim(State state, double tstop, double dt);

extern void * malloc (size_t n);
extern void exit(int n);

#endif //STATE_DIGSIM01_H
