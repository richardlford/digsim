#include <stddef.h>
#include "state.h"

void copy_state(State* dest, State* src) {
  int i = 0;
  while (i < StateSize) {
    dest->item[i] = src->item[i];
    i++;
  }
}

State* clone_state(State* data) {
  State* result = (State*)malloc(sizeof(State));
  if (!result) exit(1);
  copy_state(result, data);
  return result;
}

StateList* new_state_list(State* data, StateList* next) {
  State* data_copy = clone_state(data);
  StateList* result = (StateList*)malloc(sizeof(StateList));
  if (!result) exit(1);
  result->data = data_copy;
  result->next = next;
  return result;
}

StateList* state_list_cons(StateList* list, State* item) {
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

