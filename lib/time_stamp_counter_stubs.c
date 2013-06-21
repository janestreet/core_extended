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
#include <sys/time.h>
#include <stdint.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

#define NANOS_PER_SECOND 1000000000

/* http://en.wikipedia.org/wiki/Time_Stamp_Counter */
CAMLprim value tsc_rdtsc( )
{
#if defined(__x86_64__) || defined(__i386__)
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return Val_long( ((uint64_t)lo) | (((uint64_t)hi)<<32) );
#else
  return Val_long( 0 );
#endif
}

CAMLprim value tsc_clock_rt_getres( )
{
#ifdef __MACH__
  return Val_long( 0 );
#else
  struct timespec ts;
  if ( clock_getres( CLOCK_REALTIME, &ts ) != 0 )
    return Val_long( 0 );
  return Val_long(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
#endif
}

CAMLprim value tsc_clock_mono_getres( )
{
#ifdef __MACH__
  return Val_long( 0 );
#else
  struct timespec ts;
  if ( clock_getres( CLOCK_MONOTONIC, &ts ) != 0 )
    return Val_long( 0 );
  return Val_long(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
#endif
}

CAMLprim value tsc_clock_rt_gettime( )
{
#ifdef __MACH__ /* OS X does not have clock_gettime, use clock_get_time */
  clock_serv_t cclock;
  mach_timespec_t ts;
  if ( host_get_clock_service( mach_host_self(), CALENDAR_CLOCK, &cclock ) != KERN_SUCCESS )
    return Val_long( 0 );
  if ( clock_get_time(cclock, &ts) != KERN_SUCCESS )
    return Val_long( 0 );
  mach_port_deallocate(mach_task_self(), cclock);
  return Val_long(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
#else
  struct timespec ts;
  if ( clock_gettime( CLOCK_REALTIME, &ts ) != 0 )
    return Val_long( 0 );
  return Val_long(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
#endif
}

CAMLprim value tsc_clock_mono_gettime( )
{
#ifdef __MACH__ /* OS X does not have clock_gettime, use clock_get_time */
  clock_serv_t cclock;
  mach_timespec_t ts;
  if ( host_get_clock_service( mach_host_self(), SYSTEM_CLOCK, &cclock ) != KERN_SUCCESS )
    return Val_long( 0 );
  if ( clock_get_time(cclock, &ts) != KERN_SUCCESS )
    return Val_long( 0 );
  mach_port_deallocate(mach_task_self(), cclock);
  return Val_long(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
#else
  struct timespec ts;
  if ( clock_gettime( CLOCK_MONOTONIC, &ts ) != 0 )
    return Val_long( 0 );
  return Val_long(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
#endif
}

