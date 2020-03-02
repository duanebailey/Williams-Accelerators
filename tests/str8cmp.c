/**
 * Implementation of stateless comparison of two different strings.
 * 8 bytes of data from each string is shipped as a longword and compared.
 */
#include "rocc.h"

/* reasonable implementation of strcmp */
int swStrcmp(char *a, char *b)
{
  unsigned char c1, c2;

  do {
    c1 = *a++;
    c2 = *b++;
  } while (c1 != 0 && c1 == c2);

  return c1 - c2;
}

/**
 * Use of the stateless hardware to compare 8-byte chunks of strings at 
 * the same time.
 */
static inline unsigned long str8cmp(long int input1, long int input2)
{
  unsigned long result;
  ROCC_INSTRUCTION_DSS(0, result, input1, input2, 0);
  return result;
}

int hwStrcmp(char *a, char *b)
{
  unsigned long *s = (unsigned long*)a;
  unsigned long *t = (unsigned long*)b;
  int d;
  while ((d = str8cmp(*s,*t)) == 256) { s++; t++; }
  // sign extension
  return (d << 24) >> 24;
}

int main(void)
{
  if (str8cmp(0x0807060504030201,0x0807060004030201) != 5) return 1;
  if (str8cmp(0x0807060004030201,0x0807060504030201) != (0xff&-5)) return 2;
  if (str8cmp(0x0807060504030200,0x0807060004030201) != (0xff&-1)) return 3;
  if (str8cmp(0x0807060504030201,0x0807060004030200) != 1) return 4;
  if (str8cmp(0x0807060504030201,0x0007060504030201) != 8) return 5;
  if (str8cmp(0x0807060504030201,0x0807060504030201) != 256) return 6;
  if (str8cmp(0x0007000504030201,0x0807000504030201) != 0) return 7;

  char *a = "Now is the time for all good men.";
  char *b = "Now is the time for all good women.";
  if (hwStrcmp(a,b) != swStrcmp(a,b)) return 8;
  return 0;
}



