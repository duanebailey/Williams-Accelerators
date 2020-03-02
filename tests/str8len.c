/**
 * "Stateless" acceleration of string comparision and compare routines.
 * These hardware accelerators are used to analyze up to 8 bytes at a time.
 */
#include "rocc.h"

int swStrlen(char *s)
{
  char *end = s;
  while (*end) end++;
  return end-s;
}

/**
 * Hardware implementation of string length using stateless acceleration.
 *  make sure you include both str8len and hwStrlen.
 */
static inline unsigned long str8len(long int input)
{
  unsigned long result;
  ROCC_INSTRUCTION_DS(0, result, input, 0);
  return result;
}

int hwStrlen(char *s)
{
  unsigned long *ls = (unsigned long *)s;
  unsigned long offset;
  while ((offset=str8len(*ls)) == 8) ls++;
  return (((char*)ls)-s)+offset;
}

int main(void)
{
  /* sanity tests for the underlying hardware */
  for (int i = 0; i < 8; i++) {
    long int str = 0x0807060504030201; // string
    long int mask =0x00000000000000ff; // end-of-string location
    int l = str8len(str & ~(mask << (i<<3)));
    if (l != i) return i+1;
  }
  char *h = "hello";
  char *hw = "Hello, world!";
  // a long string:
  char *now = "now is the time for all good men to come to the aid of their country.";
  // force traversal of a different section of memory
  char *now2 = "NOW IS THE TIME FOR ALL GOOD MEN TO COME TO THE AID OF THEIR COUNTRY.";
  if (hwStrlen(h) != swStrlen(h)) return 9;
  if (hwStrlen(hw) != swStrlen(hw)) return 10;
  if (hwStrlen(now2) != swStrlen(now)) return 11;
  return 0;
}









