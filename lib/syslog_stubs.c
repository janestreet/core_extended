#include <string.h>
#include <syslog.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>

static int log_opts[] = {
  LOG_PID, LOG_CONS, LOG_ODELAY,
  LOG_NDELAY, LOG_NOWAIT, LOG_PERROR
};

CAMLprim value int_of_opt_stub(value v_option) {
  return Val_int(log_opts[Int_val(v_option)]);
}

static int log_facs[] = {
  LOG_KERN, LOG_USER, LOG_MAIL, LOG_DAEMON, LOG_AUTH, LOG_SYSLOG,
  LOG_LPR, LOG_NEWS, LOG_UUCP, LOG_CRON, LOG_AUTHPRIV, LOG_FTP,
  LOG_LOCAL0, LOG_LOCAL1, LOG_LOCAL2, LOG_LOCAL3,
  LOG_LOCAL4, LOG_LOCAL5, LOG_LOCAL6, LOG_LOCAL7
};

CAMLprim value int_of_fac_stub(value v_facility) {
  return Val_int(log_facs[Int_val(v_facility)]);
}

static int log_priorities[] = {
  LOG_EMERG, LOG_ALERT, LOG_CRIT, LOG_ERR,
  LOG_WARNING, LOG_NOTICE, LOG_INFO, LOG_DEBUG
};

CAMLprim value int_of_lev_stub(value v_priority) {
  return Val_int(log_priorities[Int_val(v_priority)]);
}

/* XXX: WARNING: this function leaks memory!  No way around that if
   syslog is called in a multi-threaded environment!
   Therefore it shouldn't be called too often.  What for, anyway? */
CAMLprim value openlog_stub(value v_ident, value v_option, value v_facility) {
  int len = caml_string_length(v_ident) + 1;
  char *ident = caml_stat_alloc(len);
  memcpy(ident, String_val(v_ident), len);
  caml_enter_blocking_section();
    openlog(ident, Int_val(v_option), Int_val(v_facility));
  caml_leave_blocking_section();
  return Val_unit;
}

CAMLprim value syslog_stub(value v_priority, value v_str) {
  int len = caml_string_length(v_str) + 1;
  char *str = caml_stat_alloc(len);
  memcpy(str, String_val(v_str), len);
  caml_enter_blocking_section();
    syslog(Int_val(v_priority), str);
    free(str);
  caml_leave_blocking_section();
  return Val_unit;
}

CAMLprim value closelog_stub() {
  closelog();
  return Val_unit;
}

CAMLprim value setlogmask_stub(value v_mask) {
  setlogmask(Int_val(v_mask));
  return Val_unit;
}
