OCAMLMAKEFILE = ../OCamlMakefile

#ifeq ($(shell uname -s), Linux)
#  LINUX_EXT_STUBS_C = linux_ext_stubs.c
#  LINUX_EXT_ML = linux_ext.mli linux_ext.ml
#	CLIBS += rt
#endif

SOURCES = \
  pp.mli \
  pp.ml \
  extended_unix.mli \
  extended_unix.ml \
  extended_string.mli \
  extended_string.ml \
  extended_sexp.mli \
  extended_sexp.ml \
  extended_result.mli \
  extended_result.ml \
  extended_option.mli \
  extended_option.ml \
  extended_list.mli \
  extended_list.ml \
  extended_linux.mli \
  extended_linux.ml \
  extended_gc.mli \
  extended_gc.ml \
  extended_float.mli \
  extended_float.ml \
  extended_filename.mli \
  extended_filename.ml \
  extended_exn.mli \
  extended_exn.ml \
  extended_array.mli \
  extended_array.ml \
  exception_check.mli \
  exception_check.ml \
  exception_sys.mli \
  exception_sys.ml \
  vector.mli \
  vector.ml \
  unix_utils.mli \
  unix_utils.ml \
  union_find.mli \
  union_find.ml \
  syslog.mli \
  syslog.ml \
  process.mli \
  process.ml \
  shell.mli \
  shell.ml \
  extended_common.mli \
  extended_common.ml \
  sendmail.mli \
  sendmail.ml \
  semaphore.mli \
  semaphore.ml \
  rw_mutex.mli \
  rw_mutex.ml \
  rmap.mli \
  rmap.ml \
  res_array.mli \
  res_array.ml \
  readline.mli \
  readline.ml \
  prompt.mli \
  prompt.ml \
  procfs.mli \
  procfs.ml \
  net_utils.mli \
  net_utils.ml \
  malloc.mli \
  malloc.ml \
  lru.mli \
  lru.ml \
  log.mli \
  log.ml \
  loggers.mli \
  loggers.ml \
  logger.mli \
  logger.ml \
  iter.mli \
  iter.ml \
  int_set.mli \
  int_set.ml \
  hashtree.mli \
  hashtree.ml \
  function.mli \
  function.ml \
  fold_table.mli \
  fold_map.mli \
  fold_map.ml \
  find.mli \
  find.ml \
  escaping.mli \
  escaping.ml \
  dispatch_arg.mli \
  dispatch_arg.ml \
  container_extended.ml \
  command.mli \
  command.ml \
  cbuffer.mli \
  cbuffer.ml \
  std.ml \
  extended_linux_stubs.c extended_unix_stubs.c malloc_stubs.c extended_sys_stubs.c syslog_stubs.c

LIB_PACK_NAME = core_extended
INCDIRS = +camlp4 .
OCAMLFLAGS = -for-pack Core_extended -dtypes
CFLAGS = \
  -pipe -g -fPIC -O2 -fomit-frame-pointer \
  -Wall -pedantic -Wextra -Wunused -Wno-long-long
PACKS = res sexplib bin_prot fieldslib core pcre
RESULT = core_extended
THREADS = yes
ANNOTATE = true

all: byte-code-library native-code-library

LIBINSTALL_FILES = \
  core_extended.cmi core_extended.cma core_extended.cmxa core_extended.a libcore_extended_stubs.a dllcore_extended_stubs.so

install:	libinstall
uninstall:	libuninstall

clean::	clean-doc

-include $(OCAMLMAKEFILE)
