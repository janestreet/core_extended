#include <stdio.h>
#include <caml/mlvalues.h>

/* See en.wikipedia.org/wiki/Binary_GCD_algorithm.

   Taken from Daniel Lemire's blog (with improvements by Ralph Corderoy):

     http://lemire.me/blog/archives/2013/12/26/fastest-way-to-compute-the-greatest-common-divisor/
*/
value core_extended_extended_int_gcd(value vu, value vv)
{
  int shift;
  unsigned long u = labs(Long_val(vu)), v = labs(Long_val(vv));
  unsigned long m;

  if ((u == 0) || (u == v)) return Val_long(v);
  if (v == 0) return Val_long(u);
  shift = __builtin_ctzl(u | v);
  u >>= __builtin_ctzl(u);
  do {
    v >>= __builtin_ctzl(v);
    m = (v ^ u) & -(v < u);
    u ^= m;
    v ^= m;
    v -= u;
  } while (v != 0);
  return Val_long(u << shift);
}
