/**
 * Test of compare/exchange low two bytes.
 */
#include "rocc.h"


unsigned long swCoEx8(long int input)
{
  unsigned char a = input & 0xff;
  unsigned char b = (input>>8) & 0xff;
  return (a<b)?(b<<8)|a:(a<<8)|b;
}

static inline unsigned long hwCoEx8(long int input)
{
  unsigned long result;
  asm volatile ("fence"); // fence instructions used for timing delimiters only
  ROCC_INSTRUCTION_DS(0, result, input, 0);
  asm volatile ("fence");  
  return result;
}

int main(void)
{
  if (hwCoEx8(0x1793) != 0x9317) return 1;
  if (hwCoEx8(0x9317) != 0x9317) return 2;
  if (swCoEx8(0x1793) != 0x9317) return 3;
  if (swCoEx8(0x9317) != 0x9317) return 4;
  return 0;
}
