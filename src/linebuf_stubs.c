/* Core_unix support functions written in C. */

#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

#include "ocaml_utils.h"


/* Replacement for broken stat functions */

static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};

#define Val_file_offset(fofs) caml_copy_int64(fofs)

static value linebuf_cst_to_constr(int n, int *tbl, int size, int deflt)
{
  int i;
  for (i = 0; i < size; i++)
    if (n == tbl[i]) return Val_int(i);
  return Val_int(deflt);
}

static value linebuf_stat_aux(struct stat *buf)
{
  CAMLparam0();
  CAMLlocal5(atime, mtime, ctime, v, offset);
  atime = caml_copy_double((double) buf->st_atime);
  mtime = caml_copy_double((double) buf->st_mtime);
  ctime = caml_copy_double((double) buf->st_ctime);
  offset = Val_file_offset(buf->st_size);
  v = caml_alloc_small(12, 0);
  Field (v, 0) = Val_int (buf->st_dev);
  Field (v, 1) = Val_int (buf->st_ino);
  Field (v, 2) = linebuf_cst_to_constr(buf->st_mode & S_IFMT, file_kind_table,
                               sizeof(file_kind_table) / sizeof(int), 0);
  Field (v, 3) = Val_int (buf->st_mode & 07777);
  Field (v, 4) = Val_int (buf->st_nlink);
  Field (v, 5) = Val_int (buf->st_uid);
  Field (v, 6) = Val_int (buf->st_gid);
  Field (v, 7) = Val_int (buf->st_rdev);
  Field (v, 8) = offset;
  Field (v, 9) = atime;
  Field (v, 10) = mtime;
  Field (v, 11) = ctime;
  CAMLreturn(v);
}

static inline char * linebuf_copy_to_c_string(value v_str)
{
  asize_t len = caml_string_length(v_str) + 1;
  char *p = caml_stat_alloc(len);
  memcpy(p, String_val(v_str), len);
  return p;
}

CAMLprim value linebuf_stat(value path)
{
  CAMLparam1(path);
  int ret;
  struct stat buf;
  char *p = linebuf_copy_to_c_string(path);
  caml_enter_blocking_section();
  ret = stat(p, &buf);
  caml_stat_free(p);
  caml_leave_blocking_section();
  if (ret == -1) uerror("stat", path);
  CAMLreturn(linebuf_stat_aux(&buf));
}
