open Core.Std

#import "config.mlh"

type t =
  | Realtime
  | Monotonic
  | Process_cpu
  | Process_thread

let all = [
  Realtime;
  Monotonic;
  Process_cpu;
  Process_thread;
]

let to_string t =
  match t with
  | Realtime       -> "Realtime"
  | Monotonic      -> "Monotonic"
  | Process_cpu    -> "Process_cpu"
  | Process_thread -> "Process_thread"

#if JSC_POSIX_TIMERS

external getres : t -> Int63.t = "caml_clock_getres" "noalloc"
external gettime : t -> Int63.t = "caml_clock_gettime" "noalloc"

module Int63_arithmetic : sig
  type t = Int63.t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
end = Int63

let min_interval t =
  let canary_val = Int63.of_int 1_000_000 in
  let current_min = ref canary_val in
  for _ = 1 to 10_000 do
    let t1 = gettime t in
    let t2 = gettime t in
    let open Int63.Replace_polymorphic_compare in
    let open Int63_arithmetic in
    if t1 <> t2 && t2 > t1 then current_min := min (t2 - t1) !current_min
  done;
  if !current_min <> canary_val then !current_min
  else failwith (Printf.sprintf !"unable to calculate min_interval for %{}" t)
;;

let mean_gettime_cost ~measure ~using =
  assert (getres Process_cpu = Int63.one);
  let count = 10_000_000 in
  let start = gettime using in
  for _ = 1 to count do
    ignore (gettime measure);
  done;
  let stop = gettime using in
  Int63_arithmetic.((stop - start) / Int63.of_int count)
;;

let getres            = Ok getres
let gettime           = Ok gettime
(* let nanosleep      = Ok nanosleep *)
let min_interval      = Ok min_interval
let mean_gettime_cost = Ok mean_gettime_cost

#else

let getres            = Or_error.unimplemented "Posix_clock.getres"
let gettime           = Or_error.unimplemented "Posix_clock.gettime"
(* let nanosleep      = Or_error.unimplemented "Posix_clock.nanosleep" *)
let min_interval      = Or_error.unimplemented "Posix_clock.min_interval"
let mean_gettime_cost = Or_error.unimplemented "Posix_clock.mean_gettime_cost"

#endif



module Time_stamp_counter = struct
  type t = int

  let diff t1 t2 = t1 - t2

#if JSC_ARCH_x86_64
    external rdtsc : unit -> int = "caml_rdtsc" "noalloc"
#elif JSC_ARCH_i386
    external rdtsc : unit -> int = "caml_rdtsc" "noalloc"
#else
    let rdtsc () =
      failwith "Posix_clock.Time_stamp_counter.rdtsc \
                is not implemented for this architecture."
#endif

#if JSC_ARCH_x86_64
    external rdtscp : unit -> int = "caml_rdtscp" "noalloc"
#elif JSC_ARCH_i386
    external rdtscp : unit -> int = "caml_rdtscp" "noalloc"
#else
    let rdtscp () =
      failwith "Posix_clock.Time_stamp_counter.rdtscp \
                is not implemented for this architecture."
#endif

end
