#include "rocc.h"

static inline void circle_init(int r)
{
	ROCC_INSTRUCTION_S(0, r, 0);
}

static inline unsigned long circle_iter()
{
	unsigned long value;
	asm volatile ("fence");	
	ROCC_INSTRUCTION_D(0, value, 1);
	return value;
}

int main(void)
{
	unsigned long x;
	circle_init(23);
        while (circle_iter());
	return 0;
}
