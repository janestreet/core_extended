open Core.Std

module Int63_arithmetic : sig
  type t = Int63.t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( * ) : t -> t -> t
end = Int63

module Test = struct
  type t =
    { name : string option;
      size : int option;
      f    : unit -> unit;
    }
  ;;

  let create ?name ?size f = { name; size; f }

  let name t = t.name
  let size t = t.size
end

module Result = struct
  module Stat = struct
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

    let empty = {
      run_time        = Int63.zero;
      run_cycles      = 0;
      gc_time         = Int63.zero;
      sample_size     = 0;
      compactions     = 0;
      minor_allocated = 0;
      major_allocated = 0;
      promoted        = 0;
    }

    let (+) a b = {
      run_time = Int63_arithmetic.(a.run_time + b.run_time);
      run_cycles  = a.run_cycles  + b.run_cycles;
      gc_time  = Int63_arithmetic.(a.gc_time  + b.gc_time);
      sample_size = a.sample_size + b.sample_size;
      compactions = a.compactions + b.compactions;
      minor_allocated = a.minor_allocated + b.minor_allocated;
      major_allocated = a.major_allocated + b.major_allocated;
      promoted    = a.promoted    + b.promoted;
    }

    let min a b = {
      run_time  = Int63.min a.run_time    b.run_time;
      run_cycles  = Int.min a.run_cycles  b.run_cycles;
      gc_time   = Int63.min a.gc_time     b.gc_time;
      sample_size = Int.min a.sample_size b.sample_size;
      compactions = Int.min a.compactions b.compactions;
      minor_allocated = Int.min a.minor_allocated b.minor_allocated;
      major_allocated = Int.min a.major_allocated b.major_allocated;
      promoted    = Int.min a.promoted    b.promoted;
    }

    let max a b = {
      run_time  = Int63.max a.run_time    b.run_time;
      run_cycles  = Int.max a.run_cycles  b.run_cycles;
      gc_time   = Int63.max a.gc_time     b.gc_time;
      sample_size = Int.max a.sample_size b.sample_size;
      compactions = Int.max a.compactions b.compactions;
      minor_allocated = Int.max a.minor_allocated b.minor_allocated;
      major_allocated = Int.max a.major_allocated b.major_allocated;
      promoted    = Int.max a.promoted    b.promoted;
    }

  end

  type t = string option * int option * Stat.t array

  let mean arr =
    let sum = Array.fold arr ~f:Stat.(+) ~init:Stat.empty in
    let n  = Array.length arr in
    let nl = Int63.of_int n   in
    { Stat.
      run_time = Int63_arithmetic.(sum.Stat.run_time / nl);
      run_cycles = sum.Stat.run_cycles / n;
      gc_time  = Int63_arithmetic.(sum.Stat.gc_time / nl);
      sample_size = sum.Stat.sample_size / n;
      compactions = sum.Stat.compactions / n;
      minor_allocated = sum.Stat.minor_allocated / n;
      major_allocated = sum.Stat.major_allocated / n;
      promoted = sum.Stat.promoted / n;
    }

  let min arr = Array.fold arr ~f:Stat.min ~init:arr.(0)
  let max arr = Array.fold arr ~f:Stat.max ~init:arr.(0)

  let sample_size arr = arr.(0).Stat.sample_size

  let compactions_occurred arr = (max arr).Stat.compactions > 0

  let minor_allocated_varied arr =
    (max arr).Stat.minor_allocated <> (min arr).Stat.minor_allocated
  let major_allocated_varied arr =
    (max arr).Stat.major_allocated <> (min arr).Stat.major_allocated
end

(* printing functions *)

let thousand = Int63.of_int 1_000
let million  = Int63.of_int 1_000_000
let billion  = Int63.of_int 1_000_000_000
let trillion = Int63.of_int64_exn 1_000_000_000_000L
let time_string =
  let open Int63_arithmetic in
  let open Int63.Replace_polymorphic_compare in
  fun ~time_format n ->
    let time_format =
      match time_format with
      | `Auto when n < million  -> `Ns
      | `Auto when n < billion  -> `Us
      | `Auto when n < trillion -> `Ms
      | `Auto                   -> `S
      | (`Ns | `Us | `Ms | `S) as fmt -> fmt
    in
    match time_format with
    | `Ns -> Int63.to_string n ^ " ns"
    | `Us -> Int63.to_string (n / thousand) ^ " us"
    | `Ms -> Int63.to_string (n / million)  ^ " ms"
    | `S  -> Int63.to_string (n / billion)  ^ " s"
;;

let make_name (name_opt, _size_opt, _results) =
  match name_opt with
  | Some name -> name
  | None -> ""
;;

let make_size (_name_opt, size_opt, _results) =
  match size_opt with
  | Some size -> Int.to_string size
  | None -> ""
;;

let make_time ~time_format (_name_opt, _size_opt, results) =
  time_string ~time_format ((Result.mean results).Result.Stat.run_time)
;;

let make_norm_time ~time_format (_name_opt, size_opt, results) =
  match size_opt with
  | Some size ->
    if size > 0 then
      let mean_time = (Result.mean results).Result.Stat.run_time in
      let size = Int63.of_int size in
      time_string ~time_format (Int63_arithmetic.(mean_time / size))
    else
      ""
  | None -> ""
;;

let make_minor_allocated (_name_opt, _size_opt, results) =
  Int.to_string (Result.mean results).Result.Stat.minor_allocated
;;

let make_major_allocated (_name_opt, _size_opt, results) =
  Int.to_string (Result.mean results).Result.Stat.major_allocated
;;

let make_promoted (_name_opt, _size_opt, results) =
  Int.to_string (Result.mean results).Result.Stat.promoted
;;

let make_cycles (_name_opt, _size_opt, results) =
  Core.Int_conversions.insert_underscores
    (Int.to_string (Result.mean results).Result.Stat.run_cycles)
;;

let make_norm_cycles (_name_opt, size_opt, results) =
  match size_opt with
  | Some size ->
    if size > 0 then
      let mean_cycles = (Result.mean results).Result.Stat.run_cycles in
      Core.Int_conversions.insert_underscores (Int.to_string (mean_cycles / size))
    else
      ""
  | None -> ""
;;

let make_warn (_name_opt, _size_opt, results) =
  let open Int63_arithmetic in
  let open Int63.Replace_polymorphic_compare in
  let twenty = Int63.of_int 20 in
  let maybe_string s predicate = if predicate then s else "" in
  let mean_run = (Result.mean results).Result.Stat.run_time in
  let min_run  = (Result.min  results).Result.Stat.run_time in
  let max_run  = (Result.max  results).Result.Stat.run_time in
  maybe_string   "m" ((mean_run - min_run) > mean_run / twenty)
  ^ maybe_string "M" ((max_run - mean_run) > mean_run / twenty)
  ^ maybe_string "c" (Result.compactions_occurred results)
  ^ maybe_string "a" (Result.minor_allocated_varied results)
  ^ maybe_string "A" (Result.major_allocated_varied results)
;;

let bars =
  match Sys.getenv "BENCH_BARS_MODE" with
  | None -> `Unicode
  | Some "ascii" -> `Ascii
  | Some "unicode" -> `Unicode
  | Some _ ->
    prerr_endline "invalid value of the environment variable BENCH_BARS_MODE.\n\
                   must be one of 'ascii' or 'unicode'.";
    exit 1
;;

type column = [ `Name
              | `Input_size
              | `Run_time
              | `Normalized_run_time
              | `Cycles
              | `Normalized_cycles
              | `Allocated
              | `Warnings ] with sexp, compare

module CMap = Map.Make (struct
  type t = column with sexp
  let compare = compare_column
end)

let default_columns = [ `If_not_empty `Name; `Normalized_cycles; `If_not_empty `Warnings ]

module Warning_set = Set.Make (struct
  type t = Char.t with sexp
  (* Case-insensitive compare, lowercase first in case of equality. *)
  let compare a b =
    match a, b with
    | ('a' .. 'z' | 'A' .. 'Z'), ('a' .. 'z' | 'A' .. 'Z') -> begin
      let a' = Char.lowercase a in
      let b' = Char.lowercase b in
      match Char.compare a' b' with
      | 0 -> Int.neg (Char.compare a b)
      | d -> d
    end
    | _ ->
      Char.compare a b
end)

let print ?(time_format=`Auto) ?(limit_width_to=72) ?(columns=default_columns) data =
  let left, right = Ascii_table.Align.(left, right) in
  (* Map displayed columns to `If_not_empty or `Yes. *)
  let displayed =
    List.fold columns ~init:CMap.empty ~f:(fun cmap column ->
      match column with
      | `If_not_empty c -> CMap.add cmap ~key:c ~data:`If_not_empty
      | #column as c    -> CMap.add cmap ~key:c ~data:`Yes)
  in
  let col tag name make align =
    Ascii_table.Column.create name make ~align
      ~show:(Option.value (CMap.find displayed tag) ~default:`No)
  in
  let columns = [
    col `Name                "Name"                 make_name                    left ;
    col `Input_size          "Input size"           make_size                    right;
    col `Run_time            "Run time"            (make_time ~time_format)      right;
    col `Normalized_run_time "Normalized run time" (make_norm_time ~time_format) right;
    col `Cycles              "Cycles"               make_cycles                  right;
    col `Normalized_cycles   "Normalized cycles"    make_norm_cycles             right;
    col `Allocated           "Allocated (minor)"    make_minor_allocated         right;
    col `Allocated           "Allocated (major)"    make_major_allocated         right;
    col `Allocated           "Promoted"             make_promoted                right;
    col `Warnings            "Warnings"             make_warn                    right;
  ] in
  Ascii_table.output ~oc:stdout ~limit_width_to ~bars columns data;
  (* Print the meaning of warnings. *)
  if CMap.mem displayed `Warnings then begin
    (* Collect used warnings. *)
    let warnings = List.fold_left data ~init:Warning_set.empty ~f:(fun cset x ->
      let warn = make_warn x in
      String.fold warn ~init:cset ~f:Warning_set.add)
    in
    if not (Warning_set.is_empty warnings) then begin
      print_string "\nWarnings:\n";
      Warning_set.iter warnings ~f:(fun c ->
        let msg = match c with
          | 'm' -> "the minimum run time was less than 80% of the mean"
          | 'M' -> "the maximum run time was more than 120% of the mean"
          | 'c' -> "GC compactions occurred during testing"
          | 'a' -> "the number of minor words allocated was not the same in all tests"
          | 'A' -> "the number of major words allocated was not the same in all tests"
          | _   -> "???"
        in
        Printf.printf "%c: %s\n" c msg)
    end
  end
;;

(* end printing functions *)

let stabilize_gc () =
  let rec loop failsafe last_heap_live_words =
    if failsafe <= 0 then
      failwith "unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.Gc.Stat.live_words <> last_heap_live_words
    then loop (failsafe - 1) stat.Gc.Stat.live_words
  in
  loop 10 0
;;

let full_major_cost ~now () =
  let count = 10 in
  let s,_ = now () in
  for _i = 1 to count do
    Gc.full_major ();
  done;
  let e,_ = now () in
  Int63_arithmetic.((e - s) / Int63.of_int count)

let find_run_size ~now gettime_cost f =
  let rec loop samples =
    let s,_ = now () in
    for _i = 1 to samples do
      f ();
    done;
    let e,_ = now () in
    (* we need enough samples so that the gettime_cost is < 1% of the cost of the run
       and we also demand that the total run take at least .5 seconds *)
    let open Int63_arithmetic in
    let open Int63.Replace_polymorphic_compare in
    if gettime_cost > (e - s) / Int63.of_int 100 || (e - s) < Int63.of_int 50_000_000
    then loop (Int.( * ) samples 2)
    else (samples, e - s)
  in
  loop 1
;;

let gc_minor_allocated gc_stat_s gc_stat_e =
  Int.of_float (gc_stat_e.Gc.Stat.minor_words -. gc_stat_s.Gc.Stat.minor_words)
;;

let gc_major_allocated gc_stat_s gc_stat_e =
  Int.of_float (gc_stat_e.Gc.Stat.major_words -. gc_stat_s.Gc.Stat.major_words)
;;

let gc_promoted gc_stat_s gc_stat_e =
  Int.of_float (gc_stat_e.Gc.Stat.promoted_words -. gc_stat_s.Gc.Stat.promoted_words)
;;

let run_once ~f ~sample_size ~gettime_cost ~full_major_cost ~now =
  let stat_s         = Gc.quick_stat () in
  let run_st, run_sc = now () in
  for _i = 1 to sample_size do
    f ();
  done;
  let run_et, run_ec = now () in
  Gc.full_major ();
  let gc_et, _ = now () in
  let stat_e = Gc.quick_stat () in
  {Result.Stat.
    run_time = Int63_arithmetic.((run_et - run_st - gettime_cost) / Int63.of_int sample_size);
    run_cycles   = (Posix_clock.Time_stamp_counter.diff run_ec run_sc) / sample_size;
    gc_time  = Int63_arithmetic.((gc_et - run_et - full_major_cost) / Int63.of_int sample_size);
    sample_size;
    compactions     = stat_e.Gc.Stat.compactions - stat_s.Gc.Stat.compactions;
    minor_allocated = gc_minor_allocated stat_s stat_e / sample_size;
    major_allocated = gc_major_allocated stat_s stat_e / sample_size;
    promoted        = gc_promoted stat_s stat_e / sample_size;
  }

let bench_basic =
  let open Core.Std.Result.Monad_infix in
  (Ok Posix_clock.Time_stamp_counter.rdtsc) >>= fun rdtsc ->
  Posix_clock.gettime           >>= fun gettime ->
  Posix_clock.mean_gettime_cost >>= fun mean_gettime_cost ->
  Posix_clock.min_interval      >>| fun min_interval ->
  fun ~verbosity ~gc_prefs ~no_compactions ~clock ~run_count test ->
  let print_high s = match verbosity with
    | `High -> printf s
    | `Low | `Mid -> ifprintf stdout s
  in
  let print_mid s = match verbosity with
    | `High | `Mid -> printf s
    | `Low -> ifprintf stdout s
  in
  let old_gc = Gc.get () in
  Option.iter gc_prefs ~f:Gc.set;
  let measurement_clock =
    match clock with
    | `Wall -> Posix_clock.Monotonic
    | `Cpu  -> Posix_clock.Process_cpu
  in
  let now () =
    let time = gettime measurement_clock in
    let tsc  = rdtsc () in
    time, tsc
  in
  if no_compactions then Gc.set { (Gc.get ()) with Gc.Control.max_overhead = 1_000_000 };
  (* calculate how long it takes us to get a time measurement for the current thread *)
  print_high "calculating cost of timing measurement: %!";
  let gettime_cost =
    mean_gettime_cost ~measure:measurement_clock ~using:Posix_clock.Monotonic
  in
  print_high "%s ns\n%!" (Int63.to_string gettime_cost);
  print_high "calculating minimal measurable interval: %!";
  let gettime_min_interval = min_interval measurement_clock in
  print_high "%s ns\n%!" (Int63.to_string gettime_min_interval);
  (* find the number of samples of f needed before gettime cost is < 1% of the total *)
  print_high "determining number of runs per sample: %!";
  let sample_size, run_time =
    (* If we're going to have a single run it makes no sense to determine sample size *)
    if run_count > 1 then
      find_run_size ~now gettime_min_interval test.Test.f
    else
      1, Int63.of_int 0
  in
  print_high "%i\n%!" sample_size;
  let runs = Array.create ~len:run_count Result.Stat.empty in
  print_high "stabilizing GC: %!";
  stabilize_gc ();
  print_high "done\n%!";
  print_high "calculating the cost of a full major sweep: %!";
  let full_major_cost = full_major_cost ~now () in
  print_high "%s ns\n%!" (Int63.to_string full_major_cost);
  print_mid "running samples for %s (estimated time %s sec)\n%!"
    (Option.value ~default:"(NO NAME)" test.Test.name)
    (Int63.to_string (Int63_arithmetic.((run_time * Int63.of_int run_count) / billion)));
  for i = 0 to run_count - 1 do
    runs.(i) <- run_once ~f:test.Test.f ~sample_size ~gettime_cost ~full_major_cost
      ~now;
    print_mid ".%!";
  done;
  print_mid "\n%!";
  (* keep f from being gc'd by calling f () again *)
  if run_count > 1 then (* It makes no sense to run multiple times slow functions *)
    test.Test.f ();
  Gc.set old_gc;
  runs

type 'a with_benchmark_flags =
  ?verbosity:[ `High | `Mid | `Low ]
  -> ?gc_prefs:Gc.Control.t
  -> ?no_compactions:bool
  -> ?fast:bool
  -> ?clock:[`Wall | `Cpu]
  -> 'a

type 'a with_print_flags =
  ?time_format:[`Ns | `Ms | `Us | `S | `Auto]
  -> ?limit_width_to:int
  -> ?columns:[ column | `If_not_empty of column ] list
  -> 'a

let default_run_count = 100

let bench_raw
    ?(verbosity=`Low) ?gc_prefs ?(no_compactions=false)
    ?(fast=false) ?(clock=`Wall) tests =
  let bench_basic = Or_error.ok_exn bench_basic in
  let run_count = if fast then 1 else default_run_count in
  List.map tests ~f:(fun test -> test.Test.name, test.Test.size,
    bench_basic ~verbosity ~gc_prefs ~no_compactions ?clock ~run_count test)
;;

let bench
    ?time_format ?limit_width_to ?columns ?verbosity ?gc_prefs ?no_compactions ?fast ?clock tests =
  print ?time_format ?limit_width_to ?columns
    (bench_raw ?verbosity ?gc_prefs ?no_compactions ?fast ?clock tests)
;;
