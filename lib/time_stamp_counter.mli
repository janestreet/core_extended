open Core.Std

(** Some experimentation with different clocks *)
external clock_rt_getres : unit -> int = "tsc_clock_rt_getres" "noalloc"
external clock_mono_getres : unit -> int = "tsc_clock_mono_getres" "noalloc"
external clock_rt_gettime : unit -> int = "tsc_clock_rt_gettime" "noalloc"
external clock_mono_gettime : unit -> int = "tsc_clock_mono_gettime" "noalloc"

(** Small routines for working with the timestamp counter.  The snapshot is machine
    specific information that is calculated once during a run and cached, though the first
    time it is requested it may take several 100 milliseconds. Cycle information captured
    on one machine can be translated to Core.Time.t on another machine only if the a
    snapshot of the first machine is also available *)
module Cycles : sig
  type t = private int with sexp, bin_io
  type snapshot with sexp, bin_io

  external now : unit -> t = "tsc_rdtsc" "noalloc"
  val add : t -> t -> t
  val diff : t -> t -> t

  val to_cycle_count : t -> int
  val of_int : int -> t

  val get_snapshot : unit -> snapshot
  val cpu_mhz : ?snapshot:snapshot -> unit -> float

  (* use information in the supplied snapshot *)
  val to_ns : ?snapshot:snapshot -> t -> int
  val to_time : ?snapshot:snapshot -> t -> Time.t
  val calibrate_snapshot : snapshot -> snapshot

  (* Recalibrate the internal snapshot maintained by this module. Recalibration at the
     rate of once in 10secs causes an error of about 1microsec. Increasing the frequency
     of recalibration does not change the precision by much. *)
  val recalibrate : unit -> unit

end
