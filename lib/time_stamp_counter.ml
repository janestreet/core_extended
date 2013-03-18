open Core.Std

module Core_time = Time ;;

external clock_rt_getres : unit -> int = "tsc_clock_rt_getres" "noalloc"
external clock_mono_getres : unit -> int = "tsc_clock_mono_getres" "noalloc"
external clock_rt_gettime : unit -> int = "tsc_clock_rt_gettime" "noalloc"
external clock_mono_gettime : unit -> int = "tsc_clock_mono_gettime" "noalloc"

module Cycles = struct

  type t = int with sexp, bin_io

  type snapshot = {
    core_time : Core_time.t;
    tsc_time : int;
    ns_per_cycle : float;
  } with sexp, bin_io

  external now : unit -> t = "tsc_rdtsc" "noalloc"

  let add  t1 t2 = t1 + t2
  let diff t1 t2 = t1 - t2
  let to_cycle_count t = t (* cycle count is just an int *)
  let of_int (t : int) : t = t  (* cycle count is just an int *)

  let get_snapshot () =
    let cycles = ref 0 in
    let secs = ref 0.0 in
    for _i = 0 to 4 do
      let tsc1 = now () in
      let t1 = Core_time.now () in
      ignore (Unix.nanosleep 0.10);
      let tsc2 = now () in
      let t2 = Core_time.now () in
      cycles := !cycles + (tsc2 - tsc1);
      secs := !secs +. (Core_time.Span.to_sec (Core_time.diff t2 t1))
    done;
    {
      core_time = Core_time.now ();
      tsc_time = now ();
      ns_per_cycle = (!secs *. 1_000_000_000.0) /. (Float.of_int !cycles);
    }
  ;;

  (* get the local cpu's snapshot *)
  let local_snapshot = ref (Lazy.lazy_from_fun get_snapshot)

  (* work with a given snapshot *)
  let to_ns ?snapshot:(ss=Lazy.force !local_snapshot) t =
    Float.iround_exn ~dir:`Nearest ((Float.of_int t) *. ss.ns_per_cycle)

  let to_time ?snapshot:(ss=Lazy.force !local_snapshot) t =
    Core_time.add ss.core_time
      (Core_time.Span.of_ns
         ((Float.of_int (t - ss.tsc_time)) *. ss.ns_per_cycle))

  let cpu_mhz ?snapshot:(ss=Lazy.force !local_snapshot) () =
    1_000_000.0 /. ss.ns_per_cycle

  (* improve a snapshot based upon TSC and Time.now changes since the time it was
     created *)
  let calibrate_snapshot snapshot =
    let tsc_now = now () in
    let time_now = Time.now () in
    let estimated_time = to_time ~snapshot tsc_now in
    let err = Time.Span.to_ns (Time.diff estimated_time time_now) in
    if (Float.abs err) < 1_000.0 (* small diffs are noise *)
    then snapshot
    else begin
      let old_tsc = snapshot.tsc_time in
      let old_ns_per_cycle = snapshot.ns_per_cycle in
      {
        (* this is to avoid negative time after a recalibration *)
        core_time = max estimated_time time_now;
        tsc_time = tsc_now;
        ns_per_cycle = old_ns_per_cycle -. err /. (Float.of_int (tsc_now - old_tsc))
      }
    end

  let recalibrate () =
    let ss = Lazy.force !local_snapshot in
    let ss = calibrate_snapshot ss in
    local_snapshot := lazy ss

end
