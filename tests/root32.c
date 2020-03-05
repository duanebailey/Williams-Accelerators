/**
 * Test of integer square root. (c) duane bailey
 * This code exercises the integer root accelerator.
 *
 * Temporarily, this computes the root of a 32 bit value, along with remainder.
 * Value sent to instruction:
 *   n(32 bits):remainder(16 bits):root(16 bits)
 */
#include "rocc.h"
// N.B. This corresponds to the MINOR value in the root32 accelerator
// must evenly divide 16 (ie: 1,2,4, or 8)
#define MINOR 4
int MAJOR = 16/MINOR; 

unsigned long swRoot(long int input)
{
  return input;
}

static inline unsigned long hwRoot(long int n)
{
  unsigned long input, result;
  int i = 0;
  input = (n<<32);
  asm volatile ("fence"); // fence instructions used for timing delimiters only
  for (i = 0; i < MAJOR; i++) {
    ROCC_INSTRUCTION_DS(0, result, input, 0);
    input = result;
  }
  asm volatile ("fence");
  result &= 0xffff;
  return result;
}

int main(void)
{
  unsigned int vals[] = { 148373646, 3622457224, 1457944990, 3191762279, 191359578, 1731124929, 2896318423, 1952794794, 198505211, 2473776291, 4019105723, 2453791028, 1063881981, 1256284020, 453441263, 1305668877, 897786541, 3558251636, 2834101364, 145720179, 284105973, 44444037, 1922077546, 1922192710, 2934143234, 1938965636, 636180022, 1223286443, 1759015301, 3652474947, 1474057579, 613021687, 4273656943, 3697814548, 3201016030, 3145771418, 3608987338, 23576941, 3164648488, 389578569, 3721005741, 2611108833, 2796484075, 81903370, 512528346, 1110218717, 1006132335, 1444973337, 2764386968, 1296588320, 2743597108, 1926467960, 1881540948, 777710239, 4058205668, 1505098496, 3488518343, 122049663, 3075351492, 2346608989, 1433298488, 2633002287, 2701077239, 2797328158, 3666212849, 2169739910, 2856458894, 2419290712, 1844376834, 2163130292, 4086478514, 2227348707, 2960708214, 987553875, 3828970456, 3957025762, 111342322, 2962300645, 4244422465, 3857561787, 1861262786, 4270112673, 2361154646, 1566550783, 3057058656, 581954695, 68856630, 997414689, 125816529, 3155327581, 3978859391, 3780097676, 425925054, 1602861543, 3104959315, 3831527145, 1375528284, 2369871624, 54299085, 3638999581 };
  unsigned int n,r,i;
  for (i = 0; i < 100; i++) {
    n = vals[i];
    r = hwRoot(n);
    printf("Root(%u)=%d\n",n,r);
    if ((r*r > n) || ((r+1)*(r+1)) <= n) return n+1;
  }
  return 0;
}
