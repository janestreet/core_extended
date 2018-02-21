open Core

[%%import "config_ext.h"]

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
} [@@deriving sexp, bin_io]

type opt =
  | TRIM_THRESHOLD
  | TOP_PAD
  | MMAP_THRESHOLD
  | MMAP_MAX
  | CHECK_ACTION
(*   | PERTURB *)
[@@deriving sexp, bin_io]

[%%ifdef JSC_LINUX_EXT]

external mallinfo     : unit -> mallinfo   = "malloc_mallinfo_stub"
external mallopt      : opt -> int -> unit = "malloc_mallopt_stub"
external malloc_trim  : int -> unit        = "malloc_trim_stub"
external malloc_stats : unit -> unit       = "malloc_stats_stub"

let mallinfo     = Ok mallinfo
let mallopt      = Ok mallopt
let malloc_trim  = Ok malloc_trim
let malloc_stats = Ok malloc_stats

[%%else]

let mallinfo     = Or_error.unimplemented "Malloc.mallinfo"
let mallopt      = Or_error.unimplemented "Malloc.mallopt"
let malloc_trim  = Or_error.unimplemented "Malloc.malloc_trim"
let malloc_stats = Or_error.unimplemented "Malloc.malloc_stats"

[%%endif]
