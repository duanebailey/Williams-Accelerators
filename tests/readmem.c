#include "rocc.h"
long readMem(char *a)
{
  unsigned long s = (unsigned long)a;
  signed long result;
  ROCC_INSTRUCTION_DS(0, result, s, 0);
  return result;
}

int main(void)
{
  char *s = "Hello world!";
  asm volatile("fence");
  int r = readMem(s);
  asm volatile("fence");
  return r != 'H';
}



