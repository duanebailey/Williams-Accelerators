#include "rocc.h"
int swTrivial()
{
  return 42;
}

int hwTrivial()
{
  signed long result;
  ROCC_INSTRUCTION_D(0, result, 0);
  return result;
}

int main(void)
{
  asm volatile("fence");
  if (hwTrivial() != swTrivial()) return 1;
  asm volatile("fence");
  return 0;
}
