#include "rocc.h"

static inline unsigned long cex(long int input)
{
	unsigned long result;
	//	asm volatile ("fence");
	ROCC_INSTRUCTION_DS(0, result, input, 0);
	return result;
}

int main(void)
{
  if (cex(0x1793) != 0x9317) return 1;
  if (cex(0x9317) != 0x9317) return 1;
  return 0;
}
