open Core.Std

(**
  Simple example:

  open Core.Std
  module Bench = Core_extended.Bench

  let main () =
    Bench.bench [Bench.Test.create ~name:"test" (fun () -> ignore (Time.now ()))]
  ;;

  let () = main ()
*)

module Test : sig
  type t
  val create : ?name:string -> ?size:int -> (unit -> unit) -> t
  val name : t -> string option
  val size : t -> int option
end

module Result : sig
  module Stat : sig
    type t = {
      run_time        : Int63.t;
      run_cycles      : int;
      gc_time         : Int63.t;
      sample_size     : int;
      compactions     : int;
      minor_allocated : int;
      major_allocated : int;
      promoted        : int;
    }

    val empty : t
  end

  type t = string option * int option * Stat.t array

  val mean : Stat.t array -> Stat.t
  val min  : Stat.t array -> Stat.t
  val max  : Stat.t array -> Stat.t

  val compactions_occurred : Stat.t array -> bool
  val sample_size : Stat.t array -> int
end

(** verbosity (default low):  If low, only prints results.  If mid, additionally prints
   time estimates and a status line.  If high, additionally prints information at each
   step of benchmarking.

   gc_prefs:  can be used to set custom GC settings during benchmarking (they will be
   reverted when the function returns)

   no_compactions (default false):  disables compactions during benchmarking, reverted
   when the function returns.  Takes precedence over gc_prefs.

   fast (default false):  run fewer tests and thus get less accurate results in less time.

   clock (default wall):  controls time measurement method.  Wall includes waiting on I/O
   and when the process is suspended/descheduled; cpu only counts time spent on
   computations.
*)

type 'a with_benchmark_flags =
  ?verbosity:[ `High | `Mid | `Low ]
  -> ?gc_prefs:Gc.Control.t
  -> ?no_compactions:bool
  -> ?fast:bool
  -> ?clock:[`Wall | `Cpu ]
  -> 'a

type column = [ `Name
              | `Input_size
              | `Run_time
              | `Normalized_run_time
              | `Cycles
              | `Normalized_cycles
              | `Allocated
              | `Warnings ]

(** [time_format] (default [`Auto]):  select the units to report run times in.  If auto, the
   units are chosen based on the times.

   The "Name" and "Input size" columns of the printed table reflect the values passed to
   Test.create.  The "Normalized" column is [run_time / input_size].  "Stdev" reports the
   standard deviation for the "Run time" column.  "Allocated" reports the average number
   of allocated words through the benchmarks.

   "Warnings" may contain single characters indicating various things:
     'm' indicates the minimum run time was less than 80% of the mean
     'M' indicates the maximum run time was more than 120% of the mean
     'c' indicates GC compactions occurred during testing
     'a' indicates the number of words allocated was not the same in all tests

   [limit_with_to] defaults to 72.

   [columns]: select which columns to display. [`If_not_empty c] means print the column
   only if at least one cell contains something. It defaults to:
     [\[`If_not_empty `Name; `Normalized_cycles; `If_not_empty `Warnings\]].
*)
type 'a with_print_flags =
  ?time_format:[`Ns | `Ms | `Us | `S | `Auto]
  -> ?limit_width_to:int
  -> ?columns:[ column | `If_not_empty of column ] list
  -> 'a

val bench : (Test.t list -> unit) with_benchmark_flags with_print_flags

(** [bench_raw] returns a list documenting the runtimes rather than printing to
   stdout. These can be fed to print for results identical to calling bench. *)
val bench_raw : (Test.t list -> Result.t list) with_benchmark_flags

val print : (Result.t list -> unit) with_print_flags
