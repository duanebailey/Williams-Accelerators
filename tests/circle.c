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
	int i;
	circle_init(23);
        for (i = 0; circle_iter() && i < 5; i++);
	circle_init(10);
        for (i = 0; circle_iter() && i < 5; i++);
	return 0;
}
