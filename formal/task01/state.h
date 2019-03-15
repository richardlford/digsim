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

typedef struct s_state_list {
  State* data;
  struct s_state_list* next;
} StateList;

void copy_state(State* dest, State* src);
State* clone_state(State* data);
StateList* new_state_list(State* data, StateList* next);
StateList* state_list_cons(StateList* list, State* item);
StateList* reverse_state_list(StateList* list);
  
// Return first derivative of state components
void diffeq(State* state_in, State* derivatives_out);

// Update state given derivatives and time delta.
void one_step(State* state, State* derivative, double dt);

// Starting from state, run the simulation until tstop,
// using dt as time step size.
// Returns the list of states.
StateList* run_sim(State* state, double tstop, double dt);

extern void * malloc (size_t n);
extern void exit(int n);

#endif //STATE_DIGSIM01_H
