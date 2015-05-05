#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <caml/mlvalues.h>

#ifdef JSC_POSIX_TIMERS
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <time.h>
#include <stdint.h>

#ifdef JSC_ARCH_SIXTYFOUR
#  define caml_alloc_int63(v) Val_long(v)
#else
#  define caml_alloc_int63(v) caml_copy_int64(v)
#endif

clockid_t caml_clockid_t_of_caml (value clock_type) {
  switch (Int_val(clock_type)) {
    case 0: return CLOCK_REALTIME;
    case 1: return CLOCK_MONOTONIC;
    case 2: return CLOCK_PROCESS_CPUTIME_ID;
    case 3: return CLOCK_THREAD_CPUTIME_ID;
  };

  caml_failwith ("invalid Clock.t");
}

value caml_clock_getres (value clock_type) {
  struct timespec tp;
  clock_getres (caml_clockid_t_of_caml (clock_type), &tp);
  return (caml_alloc_int63 (((__int64_t)tp.tv_sec * 1000 * 1000 * 1000) + (__int64_t)tp.tv_nsec));
}

value caml_clock_gettime (value clock_type) {
  struct timespec tp;
  clock_gettime (caml_clockid_t_of_caml (clock_type), &tp);
  return (caml_alloc_int63 (((__int64_t)tp.tv_sec * 1000 * 1000 * 1000) + (__int64_t)tp.tv_nsec));
}

#endif /* JSC_POSIX_TIMERS */

#if defined (JSC_ARCH_i386) || defined (JSC_ARCH_x86_64)

/* http://en.wikipedia.org/wiki/Time_Stamp_Counter */
CAMLprim value caml_rdtsc( )
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
      return Val_int( ((unsigned long long)lo)|( ((unsigned long long)hi)<<32 ));
}

CAMLprim value caml_rdtscp( )
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtscp" : "=a"(lo), "=d"(hi));
      return Val_int( ((unsigned long long)lo)|( ((unsigned long long)hi)<<32 ));
}

#endif /* JSC_ARCH_i386 || JSC_ARCH_x86_64 */
