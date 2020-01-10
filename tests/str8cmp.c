#include "rocc.h"

static inline unsigned long str8cmp(long int input1, long int input2)
{
	unsigned long result;
	ROCC_INSTRUCTION_DSS(0, result, input1, input2, 0);
	return result;
}

int main(void)
{
  if (str8cmp(0x0807060504030201,0x0807060004030201) != 5) return 1;
  if (str8cmp(0x0807060004030201,0x0807060504030201) != -5) return 1;
  if (str8cmp(0x0807060504030200,0x0807060004030201) != -1) return 1;
  if (str8cmp(0x0807060504030201,0x0807060004030200) != 1) return 1;
  if (str8cmp(0x0807060504030201,0x0007060504030201) != 8) return 1;
  if (str8cmp(0x0807060504030201,0x0807060504030201) != 0) return 1;
  if (str8cmp(0x0007000504030201,0x0807000504030201) != 0) return 1;  
  return 0;
}



