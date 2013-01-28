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
      run_time : Int63.t;
      gc_time  : Int63.t;
      sample_size : int;
      compactions : int;
      allocated : int;
    }

    let empty = {
      run_time = Int63.zero;
      gc_time  = Int63.zero;
      sample_size = 0;
      compactions = 0;
      allocated = 0;
    }

    let (+) a b = {
      run_time = Int63_arithmetic.(a.run_time + b.run_time);
      gc_time  = Int63_arithmetic.(a.gc_time  + b.gc_time);
      sample_size = a.sample_size + b.sample_size;
      compactions = a.compactions + b.compactions;
      allocated   = a.allocated   + b.allocated;
    }

    let min a b = {
      run_time  = Int63.min a.run_time    b.run_time;
      gc_time   = Int63.min a.gc_time     b.gc_time;
      sample_size = Int.min a.sample_size b.sample_size;
      compactions = Int.min a.compactions b.compactions;
      allocated   = Int.min a.allocated   b.allocated;
    }

    let max a b = {
      run_time  = Int63.max a.run_time    b.run_time;
      gc_time   = Int63.max a.gc_time     b.gc_time;
      sample_size = Int.max a.sample_size b.sample_size;
      compactions = Int.max a.compactions b.compactions;
      allocated   = Int.max a.allocated   b.allocated;
    }

  end

  type t = string option * int option * Stat.t array

  let mean arr =
    let sum = Array.fold arr ~f:Stat.(+) ~init:Stat.empty in
    let n  = Array.length arr in
    let nl = Int63.of_int n   in
    { Stat.
      run_time = Int63_arithmetic.(sum.Stat.run_time / nl);
      gc_time  = Int63_arithmetic.(sum.Stat.gc_time / nl);
      sample_size = sum.Stat.sample_size / n;
      compactions = sum.Stat.compactions / n;
      allocated = sum.Stat.allocated / n;
    }

  let min arr = Array.fold arr ~f:Stat.min ~init:arr.(0)
  let max arr = Array.fold arr ~f:Stat.max ~init:arr.(0)

  let sample_size arr = arr.(0).Stat.sample_size

  let stdev arr =
    if Array.length arr <= 1 then None else
    let mean_run = (mean arr).Stat.run_time in
    let diff_sq x y =
      let d = (Int63.to_float x) -. (Int63.to_float y) in
      d *. d
    in
    let squares = Array.map arr ~f:(fun stat -> diff_sq mean_run stat.Stat.run_time) in
    let squares_sum = Array.fold squares ~init:0. ~f:(+.) in
    let init_sd = sqrt (squares_sum /. Float.of_int (Array.length arr)) in
    Some (init_sd /. sqrt (Float.of_int (sample_size arr)))

  let compactions_occurred arr = (max arr).Stat.compactions > 0

  let allocated_varied arr =
    (max arr).Stat.allocated <> (min arr).Stat.allocated
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

let make_norm ~time_format (_name_opt, size_opt, results) =
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

let make_stdev ~time_format (_name_opt, _size_opt, results) =
  match Result.stdev results with
  | None -> "N/A"
  | Some stdev ->
    if stdev <. 100. then
      sprintf "%.3G ns" stdev
    else
      time_string ~time_format (Int63.of_float stdev)
;;

let make_allocated (_name_opt, _size_opt, results) =
  Int.to_string (Result.mean results).Result.Stat.allocated
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
  ^ maybe_string "a" (Result.allocated_varied results)
;;

let print ?(time_format=`Auto) data =
  let module Col = Ascii_table.Column in
  let right = Ascii_table.Align.right in
  let name_col = Col.create "Name" make_name in
  let size_col = Col.create ~align:right "Input size" make_size in
  let time_col = Col.create ~align:right "Run time" (make_time ~time_format) in
  let norm_col = Col.create ~align:right "Normalized" (make_norm ~time_format) in
  let stdev_col = Col.create ~align:right "Stdev" (make_stdev ~time_format) in
  let allocated_col = Col.create ~align:right "Allocated" make_allocated in
  let warn_col = Col.create ~align:right "Warnings" make_warn in

  let exists_name = List.exists data ~f:(fun (name_opt, _, _) -> is_some name_opt) in
  let exists_size = List.exists data ~f:(fun (_, size_opt, _) -> is_some size_opt) in
  Ascii_table.output ~oc:stdout
    begin
      List.concat [
        (if exists_name then [name_col] else []);
        (if exists_size then [size_col] else []);
        [time_col];
        (if exists_size then [norm_col] else []);
        [stdev_col; allocated_col; warn_col]
      ]
    end
    data
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

let full_major_cost ~now () =
  let count = 10 in
  let s = now () in
  for i = 1 to count do
    Gc.full_major ();
  done;
  let e = now () in
  Int63_arithmetic.((e - s) / Int63.of_int count)

let find_run_size ~now gettime_cost f =
  let rec loop samples =
    let s = now () in
    for i = 1 to samples do
      f ();
    done;
    let e = now () in
    (* we need enough samples so that the gettime_cost is < 1% of the cost of the run
       and we also demand that the total run take at least .5 seconds *)
    let open Int63_arithmetic in
    let open Int63.Replace_polymorphic_compare in
    if gettime_cost > (e - s) / Int63.of_int 100 || (e - s) < Int63.of_int 50_000_000
    then loop (Int.( * ) samples 2)
    else (samples, e - s)
  in
  loop 1

let gc_allocated gc_stat =
  let v =
    gc_stat.Gc.Stat.major_words
    +. gc_stat.Gc.Stat.minor_words
    -. gc_stat.Gc.Stat.promoted_words
  in
  Int.of_float v

let run_once ~f ~sample_size ~gettime_cost ~full_major_cost ~allocated_cost ~now =
  let stat_s = Gc.quick_stat () in
  let run_s = now () in
  for i = 1 to sample_size do
    f ();
  done;
  let run_e = now () in
  let stat_m = Gc.quick_stat () in
  Gc.full_major ();
  let gc_e = now () in
  let stat_e = Gc.quick_stat () in
  {Result.Stat.
    run_time = Int63_arithmetic.((run_e - run_s - gettime_cost) / Int63.of_int sample_size);
    gc_time  = Int63_arithmetic.((gc_e - run_e - full_major_cost) / Int63.of_int sample_size);
    sample_size;
    compactions = stat_e.Gc.Stat.compactions - stat_s.Gc.Stat.compactions;
    allocated = ((gc_allocated stat_m) - (gc_allocated stat_s) - allocated_cost)
                / sample_size;
  }

let allocated_cost ~now () =
  let f () = () in
  let compute ?(allocated_cost=0) sample_size=
    let result =
      run_once ~f ~sample_size ~gettime_cost:Int63.zero ~full_major_cost:Int63.zero
        ~allocated_cost ~now
    in
    result.Result.Stat.allocated
  in
  let allocated_cost = compute 1 in
  assert (0 = compute ~allocated_cost 1);
  assert (0 = compute ~allocated_cost 100);
  allocated_cost

let bench_basic =
  let open Core.Std.Result.Monad_infix in
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
  let measurement_clock = match clock with
    | `Wall -> Posix_clock.Monotonic
    | `Cpu  -> Posix_clock.Process_cpu
  in
  let now () = gettime measurement_clock in
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
  let sample_size, run_time = find_run_size ~now gettime_min_interval test.Test.f in
  print_high "%i\n%!" sample_size;
  let runs = Array.create run_count Result.Stat.empty in
  print_high "stabilizing GC: %!";
  stabilize_gc ();
  print_high "done\n%!";
  print_high "calculating the cost of a full major sweep: %!";
  let full_major_cost = full_major_cost ~now () in
  print_high "%s ns\n%!" (Int63.to_string full_major_cost);
  print_high "calculating memory allocated by tester: %!";
  let allocated_cost = allocated_cost ~now () in
  print_high "%i words\n%!" allocated_cost;
  print_mid "running samples for %s (estimated time %s sec)\n%!"
    (Option.value ~default:"(NO NAME)" test.Test.name)
    (Int63.to_string (Int63_arithmetic.((run_time * Int63.of_int run_count) / billion)));
  for i = 0 to run_count - 1 do
    runs.(i) <- run_once ~f:test.Test.f ~sample_size ~gettime_cost ~full_major_cost ~allocated_cost
      ~now;
    print_mid ".%!";
  done;
  print_mid "\n%!";
  (* keep f from being gc'd by calling f () again *)
  test.Test.f ();
  Gc.set old_gc;
  runs

type 'a with_benchmark_flags =
  ?verbosity:[ `High | `Mid | `Low ] -> ?gc_prefs:Gc.Control.t -> ?no_compactions:bool
  -> ?fast:bool -> ?clock:[`Wall | `Cpu] -> 'a

type 'a with_print_flags =
  ?time_format:[`Ns | `Ms | `Us | `S | `Auto]
  -> 'a

let default_run_count = 100

let bench_raw =
  let open Core.Std.Result.Monad_infix in
  bench_basic >>| fun bench_basic ->
  fun ?(verbosity=`Low) ?gc_prefs ?(no_compactions=false)
      ?(fast=false) ?(clock=`Wall) tests ->
    let run_count = if fast then 1 else default_run_count in
    List.map tests ~f:(fun test -> test.Test.name, test.Test.size,
      bench_basic ~verbosity ~gc_prefs ~no_compactions ?clock ~run_count test)
;;

let bench =
  let open Core.Std.Result.Monad_infix in
  bench_raw >>| fun bench_raw ->
  fun ?time_format ?verbosity ?gc_prefs ?no_compactions ?fast ?clock tests ->
    print ?time_format (bench_raw ?verbosity ?gc_prefs ?no_compactions ?fast ?clock tests)
;;
