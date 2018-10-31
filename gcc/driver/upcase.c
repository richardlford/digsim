#include "../driver/upcase.h"
CHAR upcase(CHAR inchr) {
  // This subroutine returns the uppercase representation of InChar.
  return ((inchr >= 97) && (inchr <= 122)) ? inchr - 32 : inchr;
}
