/* Core_unix support functions written in C. */

#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/syscall.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <sys/statvfs.h>
#ifndef __USE_ISOC99
# define __USE_ISOC99
#endif
#include <math.h>

#include "ocaml_utils.h"

#define MAX_ERROR_LEN 4096

CAMLprim value extended_ml_seteuid(value euid)
{
  if (seteuid(Int_val(euid))) uerror("seteuid", Nothing);
  return Val_unit;
}

CAMLprim value extended_ml_setreuid(value uid, value euid)
{
  if (setreuid(Int_val(uid),Int_val(euid))) uerror("setreuid", Nothing);
  return Val_unit;
}

CAMLprim value extended_ml_setegid(value egid)
{
  if (seteuid(Int_val(egid)) == -1) uerror("setegid", Nothing);
  return Val_unit;
}

CAMLprim value statvfs_stub (value v_path)
{
  CAMLparam1(v_path);
  CAMLlocal1(v_stat);
  struct statvfs s;
  int ret, len = caml_string_length(v_path) + 1;
  char *pathname = caml_stat_alloc(len);
  memcpy(pathname, String_val(v_path), len);
  caml_enter_blocking_section();
  ret = statvfs(pathname,&s);
  caml_leave_blocking_section();
  caml_stat_free(pathname);
  if (ret != 0) uerror("statvfs",v_path);
  v_stat = caml_alloc(11, 0);
  Store_field(v_stat, 0, Val_int(s.f_bsize));
  Store_field(v_stat, 1, Val_int(s.f_frsize));
  Store_field(v_stat, 2, Val_int(s.f_blocks));
  Store_field(v_stat, 3, Val_int(s.f_bfree));
  Store_field(v_stat, 4, Val_int(s.f_bavail));
  Store_field(v_stat, 5, Val_int(s.f_files));
  Store_field(v_stat, 6, Val_int(s.f_ffree));
  Store_field(v_stat, 7, Val_int(s.f_favail));
  Store_field(v_stat, 8, Val_int(s.f_fsid));
  Store_field(v_stat, 9, Val_int(s.f_flag));
  Store_field(v_stat,10, Val_int(s.f_namemax));
  CAMLreturn(v_stat);
}

/* copy of the ocaml's stdlib wrapper for getpid */
CAMLprim value extended_ml_gettid(value v_unit __unused)
{
  return Val_int(syscall(SYS_gettid));
}

CAMLprim value getloadavg_stub (value v_unit __unused)
{
  CAMLparam0();
  CAMLlocal1(v_ret);
  double loadavg[3];
  int ret = getloadavg(loadavg,3);
  if (ret < 0) uerror("getloadavg",Nothing);
  v_ret = caml_alloc_tuple(3);
  Store_field(v_ret, 2, caml_copy_double(ret >= 3 ? loadavg[2] : NAN));
  Store_field(v_ret, 1, caml_copy_double(ret >= 2 ? loadavg[1] : NAN));
  Store_field(v_ret, 0, caml_copy_double(ret >= 1 ? loadavg[0] : NAN));
  CAMLreturn(v_ret);
}
