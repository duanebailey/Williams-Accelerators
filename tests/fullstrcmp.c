#include "rocc.h"
#define STRCMP swstrcmp
int swstrcmp(const char* s1, const char* s2)
{
  unsigned char c1, c2;

  do {
    c1 = *s1++;
    c2 = *s2++;
  } while (c1 != 0 && c1 == c2);

  return c1 - c2;
}

long hwstrcmp(char *a, char *b)
{
  unsigned long s = (unsigned long)a;
  unsigned long t = (unsigned long)b;
  signed long result;
  ROCC_INSTRUCTION_DSS(0, result, s, t, 0);
  return result;
}

int main(void)
{
  char data[2000];
  char *dp = data;
  char *base = "123456789+123456789+123456789+123456789+123456789+";
  strcpy(dp,base);
  while (strlen(dp) < 950) {
    strcat(dp,base);
  }
  dp = data+1000;
  strcpy(dp,base);
  while (strlen(dp) < 950) {
    strcat(dp,base);
  }
  char *a = data;
  a[940] = 'x';
  char *b = data+1000;
  asm volatile("fence");
  if (STRCMP(a,b) != 71) return 1;
  asm volatile("fence");
  return 0;
}
