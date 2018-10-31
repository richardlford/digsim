#include "../driver/digtypes.h"
#include "../driver/parser.h"
#include "../driver/delim.h"

CARD tokens[MAX_NUMBER_OF_TOKENS];

int Index(CARD s, int from, int to, char c) {
  int result = from;
  for (int i = from; i <= to; i++) {
    if (s[i] == c) {
      result = i;
      break;
    }
  }
  return result;
}

void parse_card(CARD card) {
  // This subroutine parses the 80 character string in card according
  // to the delimiters in the delimiter array (any sequence of
  // delimiters is also considered a delimiter). Anything enclosed in
  // curly brackets is ignored, while quotes override other delimiters
  // to declare a token.
  //
  // The tokens (up to MAX_NUMBER_OF_TOKENS) are returned in the token
  // array.
  //
  // The tokens are passed through a common block.
  //
  // Inputs:
  //    Card - Input line to be seperated into tokens [Character*80]
  //
  // Outputs:
  //    Tokens - Parsed results from input line provided to the parser [Character*80 Array]
  //
  // Internal variables and constants:
  //    Card_Index           - Index of character in input line: being processed
  //                           [Integer]
  //    First_Char           - Index of first character of a token in the input
  //                           line (Integer]
  //    Last_Char            - Index of last character of a token in the input
  //                           line (Integer]
  //    MAX NUMBER OF TOKENS - Maximum allowable number of parsed tokens [Integer)
  //    Token                - Count of number of tokens extracted from this
  //                           command line [Integer]
  //
  //     Declare variables
  int card_index, token, first_char, last_char;
  // Initialize delimiters, comment characters, and current token
  token = -1;
  for (int i = 0; i < MAX_NUMBER_OF_TOKENS; i++)
    for (int j = 0; j < CARDSIZE; j++)
      tokens[i][j] = '\0';
  // Loop through characters in card looking for delimiters
  card_index = 0;
  while ((card_index < CARDSIZE) && (token < MAX_NUMBER_OF_TOKENS)) {
    // Check for comment section; if found look for closing brace
    if (card[card_index] == LBRACE) {
      last_char = card_index + Index(card,card_index+1,MAX_NUMBER_OF_TOKENS,RBRACE);
      if (last_char == card_index)
        card_index = CARDSIZE + 1;
      else
        card_index = last_char + 1;
    }
    //  Check for quoted section; if found look for closing quote (' or ")
    //  and write token
    else if ((card[card_index] == TICK) || (card[card_index] == QUOTE)) {
      first_char = card_index + 1;
      if (card[card_index] == TICK)
        last_char = card_index + Index(card,first_char,CARDSIZE,  TICK) - 1;
      else
        last_char = card_index + Index(card,first_char,CARDSIZE, QUOTE) - 1;
      if (last_char == card_index - 1)
        last_char = CARDSIZE;

      token = token + 1;

      // TOKENS(token)(1:(last_char-first_char+1)) = Card(first_char:last_char)
      for (int i = 0, j = first_char; j <= last_char; i++, j++) {
        tokens[token][i] = card[j];
      }
      card_index = last_char + 2;
    }
    // Check for delimiter; if found check for multiple delimeters
    else if (match_delimiter(card[card_index])) {
      while (match_delimiter(card[card_index]))
        card_index = card_index + 1;
      first_char = card_index;
      // Check for end of token and save
      while ((FALSE == match_delimiter(card[card_index])) &&
             (card[card_index] != LBRACE) &&
             ((card[card_index] != TICK) || (card[card_index] != QUOTE)) &&
             (card_index < CARDSIZE))
        card_index = card_index + 1;
      last_char = card_index - 1;
      token = token + 1;
      // TOKENS(token)(1:(last_char-first_char+1)) = Card(first_char:last_char)
      for (int i = 0, j = first_char; j <= last_char; i++, j++)
        tokens[token][i] = card[j];
      // Not a comment, quote or delimiter so must be a token. Find end of
      // token and write
    }
    else {
      first_char = card_index;
      while ((FALSE == match_delimiter(card[card_index])) &&
             (card[card_index] != LBRACE) &&
             ((card[card_index] != TICK) ||
              (card[card_index] != QUOTE)) &&
             (card_index < CARDSIZE))
        card_index = card_index + 1;

      last_char = card_index - 1;
      token = token + 1;
      // TOKENS(token)(1:(last_char-first_char+1)) = Card(first_char:last_char)
      for (int i = 0, j = first_char; j <= last_char; i++, j++)
        tokens[token][i] = card[j];
    }
    // Get rid of trailing delimiters
    while ((TRUE == match_delimiter(card[card_index])) &&
           (card_index < CARDSIZE))
      card_index = card_index + 1;
  }
}

