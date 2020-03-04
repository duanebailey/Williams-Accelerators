#include "rocc.h"

int swCircle(int r, int a[])
{
  int e = -r;
  int x = r;
  int y = 0;
  while (x >= y) {
    a[y] = x;
    e = e+2*y+1;
    y++;
    if (e >= 0) {
      e = e+1-2*x;
      x--;
    }
  }
  return y;
}

static inline void hwCircle_init(int r)
{
	ROCC_INSTRUCTION_S(0, r, 0);
}

static inline unsigned long hwCircle_iter()
{
	unsigned long value;
	ROCC_INSTRUCTION_D(0, value, 1);
	return value;
}

int hwCircle(int radius, int x[])
{
  int y = 0;
  int xVal;
  hwCircle_init(radius);
  x[y++] = radius;
  while (xVal = hwCircle_iter()) {
    x[y++] = xVal;
  }
  return y;
}

int main(void)
{
  int r = 20;
  int hwX[100];
  int swX[100];
  for (int r = 1; r < 25; r++) {
    int swN = swCircle(r,swX);
    int hwN = hwCircle(r,hwX);
    if (swN != hwN) return 1;
    for (int i = 0; i < hwN; i++) {
      if (hwX[i] != swX[i]) {
	printf("Mismatch: r=%d, sw=(%d,%d), hw=(%d,%d)\n",r,swX[i],i,hwX[i],i);
	return i+2;
      }
    }
  }
  return 0;
}
