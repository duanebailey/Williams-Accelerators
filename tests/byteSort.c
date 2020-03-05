#include "rocc.h"

static inline unsigned long hwByteSort(long int input)
{
	unsigned long result;
	//	asm volatile ("fence");
	ROCC_INSTRUCTION_DS(0, result, input, 0);
	return result;
}

int main(void)
{
  if (hwByteSort(0x0807060501020304) != 0x0807060504030201) return 1;
  if (hwByteSort(0x0807060504030102) != 0x0807060504030201) return 2;
  return 0;
}
