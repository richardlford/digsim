#ifndef PARSER
#define PARSER
#include "global.h"

#define MAX_NUMBER_OF_TOKENS 20
#define TICK '\''
#define QUOTE '"'
#define SPACE ' '
#define TAB '\t'
#define LBRACE '{'
#define RBRACE '}'

extern CARD tokens[];
extern void parse_card(CARD);

#endif
