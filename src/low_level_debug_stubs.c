#define _GNU_SOURCE

#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include "ocaml_utils.h"

static void signal_handler(int sig)
{
  char *signame = strsignal(sig);
  fprintf(stderr, "stopping process %d after signal %d (%s)\n",
          getpid(), sig, signame);
  fflush(stderr);
  kill(getpid(), SIGSTOP);
}

CAMLprim value low_level_debug_stop_upon_sigbus(value v_unit)
{
  signal(SIGBUS, signal_handler);
  return v_unit;
}

CAMLprim value low_level_debug_stop_upon_sigsegv(value v_unit)
{
  signal(SIGSEGV, signal_handler);
  return v_unit;
}

CAMLprim value low_level_debug_stop_upon_sigpipe(value v_unit)
{
  signal(SIGPIPE, signal_handler);
  return v_unit;
}

static void at_exit_handler(void)
{
  fprintf(stderr, "stopping process %d at exit\n", getpid());
  fflush(stderr);
  kill(getpid(), SIGSTOP);
}

CAMLprim value low_level_debug_stop_upon_exit(value v_unit)
{
  atexit(at_exit_handler);
  return v_unit;
}

CAMLprim value low_level_debug_stop_me_now(value v_unit)
{
  fprintf(stderr, "stopping process %d now\n", getpid());
  fflush(stderr);
  kill(getpid(), SIGSTOP);
  /* endless loop so this never terminates, otherwise
   * we might have gone past the point where we wanted
   * to stop and that makes debugging harder..
   */
  while (1);
  return v_unit;
}

/* Canary thread, detects when something is blocking the run time, and sends sigstop
   so the process can be examined. */
volatile double last_canary = 0.0;
double max_canary_wait = 0.050;

/* how often in usec to check last_canary */
long canary_check_interval = 10000;

double canary_now() {
  struct timeval tm;
  gettimeofday(&tm, NULL);
  return (tm.tv_sec + (tm.tv_usec / 1000000.0));
}

void canary_wait(int usec) {
  struct timeval tm;
  tm.tv_sec = 0;
  tm.tv_usec = usec;
  while(select(0, NULL, NULL, NULL, &tm) != 0) {}
}

/* Watch the thread that is trying to get the run time lock, if it
   doesn't succeed within max_canary_wait then send SIGSTOP to the
   current process. */
void * canary_thread(__unused void * unused) {
  double elapsed;

  while(1) {
    canary_wait(canary_check_interval);
    elapsed = canary_now() - last_canary;

    if(elapsed > max_canary_wait) {
      printf("canary blocked for %g aborting\n", elapsed);
      raise(SIGSTOP);
    }
  }

  return NULL;
}

/* never returns, starts a canary thread. The caml thread tries to
   take the caml lock every canary_check_interval, when it succeeds it
   updates last_canary. The canary thread runs every 10ms */
CAMLprim value start_canary(value v_max_wait, value v_check_interval) {
  pthread_t canary_pthread;

  last_canary = canary_now();
  max_canary_wait = Double_val(v_max_wait);
  canary_check_interval = Long_val(v_check_interval);
  printf("starting canary thread, max_wait %f check_interval %ld\n", max_canary_wait, canary_check_interval);

  pthread_create(&canary_pthread, NULL, &canary_thread, NULL);

  while(1) {
    caml_enter_blocking_section();
    canary_wait(canary_check_interval);
    caml_leave_blocking_section();
    last_canary = canary_now();
  }

  return Val_unit;
}
