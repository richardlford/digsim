#include <stddef.h>
#include "state.h"

StateList* new_state_list(State data, StateList* next) {
  StateList* result = (StateList*)malloc(sizeof(StateList));
  if (!result) exit(1);
  result->data = data;
  result->next = next;
  return result;
}

StateList* state_list_cons(StateList* list, State item) {
  StateList* cell = new_state_list(item, list);
  return cell;
}

StateList* reverse_state_list(StateList* list) {
  StateList* v = list;
  StateList* w = NULL;
  while (v != NULL) {
    StateList* t = v->next;
    v->next = w;
    w = v;
    v = t;
  }
  return w;
}

