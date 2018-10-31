#include <stdio.h>
#include <stdlib.h>
#include "../driver/global.h"
#include "../driver/sysvars.h"
#include "../driver/parser.h"
#include "../driver/print.h"
void print_card() {
  // This subroutine decodes a "PRINT" card; which adds the specified
  // variable to the list of those to be printed.  The format is:
  //
  // PRINT (Variable) (Location)
  //
  // Inputs:
  //    Number Of Prints - Number of communications array locations to print
  //                       [Integer]
  //    Tokens           - Parsed results from input line provided to the parser
  //                       [Character*80 Array]
  //
  // Outputs:
  //    Number Of Prints - Number of communications array locations to print
  //                       [Integer]
  //    Print Index      - Communications array location of print data
  //                       [Integer Array]
  //
  // Internal variables and constants:
  //    MAX NUMBER OF PRINTS - Maximum allowable number of communications array
  //                           location to print (Integer]
  //    Var_Loc              - Communications array index of variable to
  //                           be printed (Integer]
  //
  // Include common block definitions
  //
  //   Exceptions to default type
  int var_loc;
  // If there's space left
  if (number_of_prints < MAX_NUMBER_OF_PRINTS ) {
    //     Get location
    //jlc3    Decode((Index(Tokens(3), ' ') - 1), '(I5)', Tokens(3)) Var_Loc
    var_loc = atoi(tokens[2]);
    //  Set global common index
    print_index[number_of_prints] = var_loc;
    number_of_prints = number_of_prints + 1;
    // If there is no more space, write error message to output file
  }
  else {
    printf(" Too many PRINT commands\n");
    printf(" PRINT command not executed");
  }
  // Exit to Input_Data
}
