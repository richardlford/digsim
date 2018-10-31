#include "../driver/global.h"
BOOL match_delimiter(CHAR inchar) {
  // Indicates input character is one of the stored delimiters [Logical]
  CHAR delimiters[4] = {' ' , '=' , ';' , ':'};
  BOOL result = FALSE;
  for (int i = 0; i < 4; i++) {
    if (delimiters[i] == inchar) {
      result = TRUE;
      break;
    }
  }
  return result;
}
