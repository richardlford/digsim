#include <stddef.h>
#include "state.h"

StateList* new_state_list() {
  StateList* result = (StateList*)malloc(sizeof(StateList));
  if (!result) exit(1);
  result->first = NULL;
  result->last = NULL;
  return result;
}

StateListCell* new_state_list_cell(State data, StateListCell* next) {
  StateListCell* result = (StateListCell*)malloc(sizeof(StateListCell));
  if (!result) exit(1);
  result->data = data;
  result->next = next;
  return result;
}

void state_list_append(StateList* list, State item) {
  StateListCell* cell = new_state_list_cell(item, NULL);
  if (list->first == NULL) {
    list->first = cell;
  } else {
    list->last->next = cell;
  }
  list->last = cell;
}

