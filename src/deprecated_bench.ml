(** DEPRECATED: use the base/bench library instead *)

open Core.Std
open Textutils.Std

module Int63_arithmetic : sig
  type t = Int63.t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
end = Int63

module Test = struct
  type t =
    { name : string option;
      size : int;
      f    : unit -> unit;
    }
  ;;

  let create ?name ?(size = 1) f = { name; size; f }
  let name t = t.name
  let size t = t.size
end

module Result = struct
  module Stat = struct
    type t = {
      run_cycles      : int;
      compactions     : int;
      minor_allocated : int;
      major_allocated : int;
      promoted        : int;
    }

    let empty = {
      run_cycles      = 0;
      compactions     = 0;
      minor_allocated = 0;
      major_allocated = 0;
      promoted        = 0;
    }

    let (+) a b = {
      run_cycles  = a.run_cycles  + b.run_cycles;
      compactions = a.compactions + b.compactions;
      minor_allocated = a.minor_allocated + b.minor_allocated;
      major_allocated = a.major_allocated + b.major_allocated;
      promoted    = a.promoted    + b.promoted;
    }

    let min a b = {
      run_cycles  = Int.min a.run_cycles  b.run_cycles;
      compactions = Int.min a.compactions b.compactions;
      minor_allocated = Int.min a.minor_allocated b.minor_allocated;
      major_allocated = Int.min a.major_allocated b.major_allocated;
      promoted    = Int.min a.promoted    b.promoted;
    }

    let max a b = {
      run_cycles  = Int.max a.run_cycles  b.run_cycles;
      compactions = Int.max a.compactions b.compactions;
      minor_allocated = Int.max a.minor_allocated b.minor_allocated;
      major_allocated = Int.max a.major_allocated b.major_allocated;
      promoted    = Int.max a.promoted    b.promoted;
    }

  end

  type t = string option * int * Stat.t array

  let mean arr =
    let sum = Array.fold arr ~f:Stat.(+) ~init:Stat.empty in
    let n  = Array.length arr in
    { Stat.
      run_cycles = sum.Stat.run_cycles / n;
      compactions = sum.Stat.compactions / n;
      minor_allocated = sum.Stat.minor_allocated / n;
      major_allocated = sum.Stat.major_allocated / n;
      promoted = sum.Stat.promoted / n;
    }

  let min arr = Array.fold arr ~f:Stat.min ~init:arr.(0)
  let max arr = Array.fold arr ~f:Stat.max ~init:arr.(0)

  let compactions_occurred arr = (max arr).Stat.compactions > 0

  let minor_allocated_varied arr =
    (max arr).Stat.minor_allocated <> (min arr).Stat.minor_allocated
  let major_allocated_varied arr =
    (max arr).Stat.major_allocated <> (min arr).Stat.major_allocated
end

(* printing functions *)

let make_name (name_opt, _size, _results) =
  match name_opt with
  | Some name -> name
  | None -> ""
;;

let make_size (_name_opt, size, _results) =
  Int.to_string size
;;

let make_minor_allocated (_name_opt, _size, results) =
  Int.to_string (Result.mean results).Result.Stat.minor_allocated
;;

let make_major_allocated (_name_opt, _size, results) =
  Int.to_string (Result.mean results).Result.Stat.major_allocated
;;

let make_promoted (_name_opt, _size, results) =
  Int.to_string (Result.mean results).Result.Stat.promoted
;;

let make_cycles (_name_opt, _size, results) =
  Int_conversions.insert_underscores
    (Int.to_string (Result.mean results).Result.Stat.run_cycles)
;;

let make_norm_cycles (_name_opt, size, results) =
  if size > 0 then
    let mean_cycles = (Result.mean results).Result.Stat.run_cycles in
    Int_conversions.insert_underscores (Int.to_string (mean_cycles / size))
  else
    ""
;;

let make_nanos (_name_opt, size, results) =
  let module Tsc_span = Time_stamp_counter.Span in
  if size <= 0 then
    ""
  else
    let mean_cycles = (Result.mean results).Result.Stat.run_cycles in
    let nanos =
      Float.to_int
        (Time.Span.to_ns
           (Tsc_span.to_time_span (Tsc_span.of_int_exn (mean_cycles / size))))
    in
    Int_conversions.insert_underscores (Int.to_string nanos)
;;

let make_warn (_name_opt, _size, results) =
  let twenty = 20 in
  let maybe_string s predicate = if predicate then s else "" in
  let mean_run = (Result.mean results).Result.Stat.run_cycles in
  let min_run  = (Result.min  results).Result.Stat.run_cycles in
  let max_run  = (Result.max  results).Result.Stat.run_cycles in
  maybe_string   "m" ((mean_run - min_run) > mean_run / twenty)
  ^ maybe_string "M" ((max_run - mean_run) > mean_run / twenty)
  ^ maybe_string "c" (Result.compactions_occurred results)
  ^ maybe_string "a" (Result.minor_allocated_varied results)
  ^ maybe_string "A" (Result.major_allocated_varied results)
;;

type column = [ `Name
              | `Input_size
              | `Cycles
              | `Normalized_cycles
              | `Nanos
              | `Allocated
              | `Warnings ] [@@deriving sexp, compare]

module CMap = Map.Make (struct
  type t = column [@@deriving sexp]
  let compare = compare_column
end)

let default_columns = [ `If_not_empty `Name; `Normalized_cycles; `If_not_empty `Warnings ]

module Warning_set = Set.Make (struct
  type t = Char.t [@@deriving sexp]
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

let print ?(limit_width_to=72) ?(columns=default_columns) ?display data =
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
    col `Name                "Name"                 make_name                    Left ;
    col `Input_size          "Input size"           make_size                    Right;
    col `Cycles              "Cycles"               make_cycles                  Right;
    col `Normalized_cycles   "Normalized cycles"    make_norm_cycles             Right;
    col `Nanos               "Time (ns)"            make_nanos                   Right;
    col `Allocated           "Allocated (minor)"    make_minor_allocated         Right;
    col `Allocated           "Allocated (major)"    make_major_allocated         Right;
    col `Allocated           "Promoted"             make_promoted                Right;
    col `Warnings            "Warnings"             make_warn                    Right;
  ] in
  Ascii_table.output ?display ~oc:stdout ~limit_width_to columns data;
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
  let s = now () in
  for _ = 1 to count do
    Gc.full_major ();
  done;
  let e = now () in
  Int63_arithmetic.((e - s) / Int63.of_int count)

let find_run_size ~now ~rdtscp ~mean_cycles f =
  let rec loop samples =
    let s = now () in
    let sc = rdtscp () in
    for _ = 1 to samples do
      f ();
    done;
    let ec = rdtscp () in
    let e = now () in
    (* we need enough samples so that the mean_cycles is < 1% of the cost of the run
       and we also demand that the total run take at least .01 seconds *)
    if mean_cycles * 100 > (Posix_clock.Time_stamp_counter.diff ec sc)
      || Int63.Replace_polymorphic_compare.(Int63_arithmetic.(e - s) < Int63.of_int 1_000_000)
    then loop (Int.( * ) samples 2)
    else samples
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

let run_once ~f ~mean_cycles ~rdtscp =
  let stat_s = Gc.quick_stat () in
  f (); (* call f once to warm up the cache *)
  let cycles_now = rdtscp () in
  f ();
  let cycles_after = rdtscp () in
  let stat_e = Gc.quick_stat () in
  {Result.Stat.
    run_cycles = ((Posix_clock.Time_stamp_counter.diff cycles_after cycles_now) - mean_cycles);
    compactions     = stat_e.Gc.Stat.compactions - stat_s.Gc.Stat.compactions;
    minor_allocated = gc_minor_allocated stat_s stat_e;
    major_allocated = gc_major_allocated stat_s stat_e;
    promoted        = gc_promoted stat_s stat_e;
  }

let time_cycles ~rdtscp =
  let rec loop n lst =
    match n with
    | 0 -> lst
    | n ->
      let start = rdtscp () in
      let after = rdtscp () in
      loop (n-1) ((Posix_clock.Time_stamp_counter.diff after start)::lst)
  in
  let times = loop 100000 [] in
  (List.fold_left ~f:(+) ~init:0 times) / (List.length times)

let bench_basic =
  let open Core.Std.Result.Monad_infix in
  (Ok Posix_clock.Time_stamp_counter.rdtsc) >>= fun rdtscp ->
  Posix_clock.gettime           >>= fun gettime ->
  Posix_clock.mean_gettime_cost >>= fun mean_gettime_cost ->
  Posix_clock.min_interval      >>| fun min_interval ->
  fun ~verbosity ~gc_prefs ~no_compactions ~clock ~trials test ->
  let print_high s = match verbosity with
    | `High -> printf s
    | `Low | `Mid -> Printf.ifprintf stdout s
  in
  let print_mid s = match verbosity with
    | `High | `Mid -> printf s
    | `Low -> Printf.ifprintf stdout s
  in
  print_mid "\n===== running test: %s ======\n%!" (Option.value ~default:"(NO NAME)" test.Test.name);
  let old_gc = Gc.get () in
  Option.iter gc_prefs ~f:Gc.set;
  let measurement_clock =
    match clock with
    | `Wall -> Posix_clock.Monotonic
    | `Cpu  -> Posix_clock.Process_cpu
  in
  let now () = gettime measurement_clock in
  let mean_cycles = time_cycles ~rdtscp in
  print_high "cost of running rdtscp: %d cycles\n%!" mean_cycles;
  if no_compactions then Gc.set { (Gc.get ()) with Gc.Control.max_overhead = 1_000_000 };
  (* calculate how long it takes us to get a time measurement for the current thread *)
  let gettime_cost =
    mean_gettime_cost ~measure:measurement_clock ~using:Posix_clock.Monotonic
  in
  print_high "calculating cost of timing measurement: %s ns\n%!"
    (Int63.to_string gettime_cost);
  let gettime_min_interval = min_interval measurement_clock in
  print_high "calculating minimal measurable interval: %s ns\n%!"
    (Int63.to_string gettime_min_interval);
  (* find the number of samples of f needed before gettime cost is < 1% of the total *)
  print_high "determining number of trials: %!";
  let run_count = match trials with
    | `Auto -> find_run_size ~now ~mean_cycles ~rdtscp test.Test.f
    | `Num n -> n
  in
  print_high "%d\n%!" run_count;
  let runs = Array.create ~len:run_count Result.Stat.empty in
  print_high "stabilizing GC: %!";
  stabilize_gc ();
  print_high "done\n%!";
  let full_major_cost = full_major_cost ~now () in
  print_high "calculating the cost of a full major sweep: %s ns\n%!"
    (Int63.to_string full_major_cost);
  for i = 0 to run_count - 1 do
    runs.(i) <- run_once ~f:test.Test.f ~mean_cycles ~rdtscp;
    print_mid "\r(%d / %d)%!" (i + 1) (run_count)
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
  -> ?trials:[ `Auto | `Num of int ]
  -> ?clock:[`Wall | `Cpu]
  -> 'a

type 'a with_print_flags =
  ?limit_width_to:int
  -> ?columns:[ column | `If_not_empty of column ] list
  -> ?display:Ascii_table.Display.t
  -> 'a

let bench_raw
    ?(verbosity=`Low) ?gc_prefs ?(no_compactions=false)
    ?(trials=`Auto) ?(clock=`Wall) tests =
  let bench_basic = Or_error.ok_exn bench_basic in
  List.map tests ~f:(fun test -> test.Test.name, test.Test.size,
    bench_basic ~verbosity ~gc_prefs ~no_compactions ~clock ~trials test)
;;

let bench
    ?limit_width_to ?columns ?display ?verbosity ?gc_prefs ?no_compactions ?trials ?clock tests =
  print ?limit_width_to ?columns ?display
    (bench_raw ?verbosity ?gc_prefs ?no_compactions ?trials ?clock tests)
;;
