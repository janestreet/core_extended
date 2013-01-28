open Core.Std

INCLUDE "config.mlh"

type mallinfo = {
  arena : int;
  ordblks : int;
  smblks : int;
  hblks : int;
  hblkhd : int;
  usmblks : int;
  fsmblks : int;
  uordblks : int;
  fordblks : int;
  keepcost : int;
} with sexp, bin_io

type opt =
  | TRIM_THRESHOLD
  | TOP_PAD
  | MMAP_THRESHOLD
  | MMAP_MAX
  | CHECK_ACTION
(*   | PERTURB *)
with sexp, bin_io

IFDEF LINUX_EXT THEN

external mallinfo     : unit -> mallinfo   = "malloc_mallinfo_stub"
external mallopt      : opt -> int -> unit = "malloc_mallopt_stub"
external malloc_trim  : int -> unit        = "malloc_trim_stub"
external malloc_stats : unit -> unit       = "malloc_stats_stub"

let mallinfo     = Ok mallinfo
let mallopt      = Ok mallopt
let malloc_trim  = Ok malloc_trim
let malloc_stats = Ok malloc_stats

ELSE

let mallinfo     = unimplemented "Malloc.mallinfo"
let mallopt      = unimplemented "Malloc.mallopt"
let malloc_trim  = unimplemented "Malloc.malloc_trim"
let malloc_stats = unimplemented "Malloc.malloc_stats"

ENDIF
