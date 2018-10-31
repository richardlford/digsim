#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../driver/input.h"
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/parser.h"
#include "../driver/upcase.h"
#include "../driver/prn_card.h"
#include "../driver/set_card.h"

extern FILE *input;

// getline: get line into s, return length
void read(FILE *input, CARD card) {
  int i;
  CHAR c;
  for (i = 0; i < CARDSIZE; i++) card[i] = '\0';
  i = 0;
  while ((c = fgetc(input)) != EOF) {
    if (c == '\n') return;
    if (i >= CARDSIZE) continue;
    card[i] = c;
    i++;
  }
}

BOOL input_data(FILE *input) {
  // This subroutine reads the input data cards; then calls the appropriate
  // processing routine (or, for simple commands, performs the processing
  // directly).
  //
  // Commands & command formats:
  //
  //     PRINT - Prints selected variable
  //         PRINT {Variable} {Location)
  //     SET   - Assigns a value to a variable prior to initialization
  //         SET {Variable} = {Value} {Location}
  //     RUN   - Runs the simulation and returns for more input
  //         RUN
  //     STOP  - Stops the driver
  //         STOP
  BOOL execute_run;  // Indicates that a run is to be executed [Logical]
  CHAR card[CARDSIZE];     // Command line from input data file [Character*80]
  CHAR command[CARDSIZE];  // Uppercase copy of first token [Character*80]
  // Begin loop to read in cards
  execute_run = FALSE;
  while (TRUE) {
    // Read in next card and parse card into tokens
    read(input,card); // READ (99,'(A80)',END=50) card;
    parse_card(card);
    for (int i = 0; i <= 5; i++) command[i] = upcase(tokens[0][i]);
    // Call decoding routine for this card
    if (!strcmp(command,"PRINT")) print_card();
    else if (!strcmp(command,"RUN" )) {
      execute_run = TRUE;
      break;
    }
    else if (!strcmp(command,"SET")) set_card();
    else if (!strcmp(command,"STOP")) {
      execute_run = FALSE;
      break;
    }
    // Comment card, take no action
    else if (!strcmp(command," ")) {
      // Card matches none of the above, error condition
      printf(" Unreadable input card\n>>>>%s",card);
      break;
    }
  }
  return execute_run;
}
