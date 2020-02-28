#include "rocc.h"

int swStrlen(char *s)
{
  char *end = s;
  while (*end++);
  return end-s;
}

static inline unsigned long str8len(long int input)
{
  unsigned long result;
  ROCC_INSTRUCTION_DS(0, result, input, 0);
  return result;
}

int hwStrlen(char *s)
{
  unsigned long *ls = (unsigned long *)s;
  unsigned long l8;
  unsigned long x;
  while ((x=str8len(*ls)) == 8) ls++;
  return (((char*)ls)-s)+x;
}

int main(void)
{
  if (str8len(0x0807060004030201) != 4) return 1;
  if (str8len(0x0800060504030201) != 6) return 1;
  if (str8len(0x0807060054030200) != 0) return 1;
  if (str8len(0x0807060504030201) != 8) return 1;
  char *h = "hello";
  char *hw = "Hello, world!";
  char *now = "Now is the time for all good men to come to the aid of their country.";
  char *now2 = "Now is the time for all good men to come to the aid of their country.";
  if (hwStrlen(h) != swStrlen(h)) return 1;
  if (hwStrlen(hw) != swStrlen(hw)) return 1;
  if (hwStrlen(now2) != swStrlen(now)) return 1;
  return 0;
}
