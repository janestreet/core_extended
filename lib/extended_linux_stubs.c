#include "config.h"

#ifdef JSC_LINUX_EXT

#define _GNU_SOURCE

#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/epoll.h>
#include <sys/vfs.h>

#include "ocaml_utils.h"
#include "unix_utils.h"

/* resuid */

CAMLprim value linux_getresuid_stub(value __unused v_unit)
{
  value v_res;
  uid_t ruid,euid,suid;

  if (getresuid(&ruid, &euid, &suid) == -1) uerror("getresuid", Nothing);

  v_res = caml_alloc_small(3, 0);
  Field(v_res, 0) = Val_int(ruid);
  Field(v_res, 1) = Val_int(euid);
  Field(v_res, 2) = Val_int(suid);

  return v_res;
}

CAMLprim value linux_setresuid_stub(value v_ruid, value v_euid, value v_suid)
{
  if (setresuid(Int_val(v_ruid), Int_val(v_euid), Int_val(v_suid)) == -1)
    uerror("setresuid", Nothing);

  return Val_unit;
}


/* Epoll */
/* Epoll functions where removed after 80221ddbe375
   HG cat this file at this revision if you need to resuscitate them
 */
/* Splicing - zero-copies between kernel buffers */

CAMLprim value linux_splice_make_flags_stub(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= SPLICE_F_MOVE; break;
      case 1 : flags |= SPLICE_F_NONBLOCK; break;
      case 2 : flags |= SPLICE_F_MORE; break;
      default : flags |= SPLICE_F_GIFT; break;
    }
  }
  return caml_copy_int32(flags);
}

CAMLprim value linux_splice_stub(
  value v_assume_fd_is_nonblocking,
  value v_fd_in, value v_off_in,
  value v_fd_out, value v_off_out,
  value v_len, value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd_in = Int_val(v_fd_in);
  int fd_out = Int_val(v_fd_out);
  off64_t off_in = Long_val(v_off_in);
  off64_t *off_in_p = (off_in < 0) ? NULL : &off_in;
  off64_t off_out = Long_val(v_off_out);
  off64_t *off_out_p = (off_out < 0) ? NULL : &off_out;
  size_t len = Long_val(v_len);
  unsigned int flags = Int32_val(v_flags);
  long ret;
  value v_res;

  if (assume_fd_is_nonblocking)
    ret = splice(fd_in, off_in_p, fd_out, off_out_p, len, flags);
  else {
    caml_enter_blocking_section();
      ret = splice(fd_in, off_in_p, fd_out, off_out_p, len, flags);
    caml_leave_blocking_section();
  }

  if (ret == -1) uerror("splice", Nothing);

  v_res = caml_alloc_small(3, 0);
  Field(v_res, 0) = Val_long(ret);
  Field(v_res, 1) = Val_long(off_in);
  Field(v_res, 2) = Val_long(off_out);

  return v_res;
}

CAMLprim value linux_splice_stub_bc(value *argv, int __unused argn)
{
  return linux_splice_stub(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value linux_tee_stub(
  value v_assume_fd_is_nonblocking,
  value v_fd_in, value v_fd_out,
  value v_len, value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd_in = Int_val(v_fd_in);
  int fd_out = Int_val(v_fd_out);
  size_t len = Long_val(v_len);
  unsigned int flags = Int32_val(v_flags);
  long ret;

  if (assume_fd_is_nonblocking)
    ret = tee(fd_in, fd_out, len, flags);
  else {
    caml_enter_blocking_section();
      ret = tee(fd_in, fd_out, len, flags);
    caml_leave_blocking_section();
  }

  if (ret == -1) uerror("tee", Nothing);

  return Val_long(ret);
}

CAMLprim value linux_vmsplice_stub(
  value v_assume_fd_is_nonblocking,
  value v_fd, value v_iovecs, value v_count,
  value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd = Int_val(v_fd);
  int count = Int_val(v_count);
  size_t total_len = 0;
  struct iovec *iovecs = copy_iovecs(&total_len, v_iovecs, count);
  unsigned int flags = Int32_val(v_flags);
  long ret;

  if (assume_fd_is_nonblocking && total_len < THREAD_IO_CUTOFF)
    ret = vmsplice(fd, iovecs, count, flags);
  else {
    Begin_roots1(v_iovecs);
    caml_enter_blocking_section();
      ret = vmsplice(fd, iovecs, count, flags);
    caml_leave_blocking_section();
    End_roots();
  }

  if (ret == -1) uerror("vmsplice", Nothing);

  return Val_long(ret);
}

CAMLprim value linux_statfs_stub(value v_path)
{
  CAMLparam1(v_path);
  CAMLlocal1(res);

  struct statfs sfs;
  memset(&sfs, 0, sizeof sfs);

  if (statfs(String_val(v_path), &sfs) == -1)
    uerror("statfs", Nothing);

  res = caml_alloc_tuple(8);
  Store_field(res, 0, caml_copy_int32(sfs.f_type));
  Store_field(res, 1, Val_long(sfs.f_bsize));
  Store_field(res, 2, Val_long(sfs.f_blocks));
  Store_field(res, 3, Val_long(sfs.f_bfree));
  Store_field(res, 4, Val_long(sfs.f_bavail));
  Store_field(res, 5, Val_long(sfs.f_files));
  Store_field(res, 6, Val_long(sfs.f_ffree));
  Store_field(res, 7, Val_long(sfs.f_namelen));
  CAMLreturn(res);
}

#endif /* JSC_LINUX_EXT */
