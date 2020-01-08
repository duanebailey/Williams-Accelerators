#include "rocc.h"

static inline unsigned long fortyTwo()
{
	unsigned long result;
	//	asm volatile ("fence");
	ROCC_INSTRUCTION_D(0, result, 0);
	return result;
}

int main(void)
{
  unsigned long ft = fortyTwo();
  if (ft != 42)
    return ft;
  return 10;
}
