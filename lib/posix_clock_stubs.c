#include "config.h"
#ifdef JSC_POSIX_TIMERS

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <time.h>

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

/*
value caml_clock_nanosleep (value clock_type, value nanoseconds_v) {
  CAMLparam2 (clock_type, nanoseconds_v);
  caml_enter_blocking_section ();

  struct timespec tp, remaining;
  clockid_t clockid = caml_clockid_t_of_caml (clock_type);

  clock_getres (clockid, &tp);
  tp.tv_sec = 0;
  tp.tv_nsec = Int_val(nanoseconds_v);
  tp.tv_nsec = tp.tv_nsec + Int_val(nanoseconds_v);

  while (1 == 1) {
    if (clock_nanosleep (clockid, 0, &tp, &remaining) == 0) {
      if (remaining.tv_sec == 0 && remaining.tv_nsec == 0) {
        caml_leave_blocking_section ();
        CAMLreturn (Val_unit);
      }
      else {
        printf ("%i, %i\n", remaining.tv_sec, remaining.tv_nsec);
      };
    }
    else caml_failwith ("call to clock_nanosleep failed");
  };
}
*/

#endif /* JSC_POSIX_TIMERS */
