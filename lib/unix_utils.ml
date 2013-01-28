open Core.Std

module RLimit = Unix.RLimit

(* Handling RAM limits *)

let physical_ram () =
  Int64.( * ) (Unix.sysconf Unix.PAGESIZE) (Unix.sysconf Unix.PHYS_PAGES)

type ram_usage_limit = Unlimited | Absolute of int64 | Relative of float

let set_ram_limit l =
  RLimit.set `Virtual_memory
    {
      RLimit.cur = RLimit.Limit l;
      RLimit.max = RLimit.Infinity;
    }

let apply_ram_usage_limit = function
  | Unlimited -> ()
  | Absolute i -> set_ram_limit i
  | Relative f ->
      set_ram_limit (Int64.of_float (f *. Int64.to_float (physical_ram ())))

let ram_msg =
  "RAM limit should be either an integer or a float between 0 and 1, not "

let string_to_ram_usage_limit s =
  try
    let i = Int64.of_string s in
    if i > Int64.zero then Absolute i
    else Unlimited
  with Failure _ ->
    let f = float_of_string s in
    if f < 0. || f > 1. then raise (Arg.Bad (ram_msg ^ s));
    if f > 0. then Relative f
    else Unlimited

let ram_limit_spec =
  (
    "-ram_limit",
    Arg.String (fun s -> apply_ram_usage_limit (string_to_ram_usage_limit s)),
    "num Limit RAM consumption either as an absolute number of bytes or as \
     a fraction of the total RAM, 0 - no limit"
  )


(* Signal handling *)

let all_sigs =
  [
    Signal.abrt;
    Signal.alrm;
    Signal.fpe;
    Signal.hup;
    Signal.ill;
    Signal.int;
    Signal.kill;
    Signal.pipe;
    Signal.quit;
    Signal.segv;
    Signal.term;
    Signal.usr1;
    Signal.usr2;
    Signal.chld;
    Signal.cont;
    Signal.stop;
    Signal.tstp;
    Signal.ttin;
    Signal.ttou;
    Signal.vtalrm;
    Signal.prof;
  ]

let wrap_block_signals f =
  let blocked_sigs = Signal.sigprocmask `Set all_sigs in
  protect ~f ~finally:(fun () ->
    ignore (Signal.sigprocmask `Set blocked_sigs))


(* at_exit functions are honored only when terminating by exit, not by signals,
   so we need to do some tricks to get it run by signals too.
   NB: Ctrl-C is _not_ handled by this function, i.e., it terminates a program
   without running at_exit functions. *)
let ensure_at_exit () =
  let pid = Unix.getpid () in
  let handler signal =
    do_at_exit ();
    Signal.set signal `Default;
    Signal.send_i signal (`Pid pid)
  in
  List.iter ~f:(fun s -> Signal.set s (`Handle handler)) [
    (* there are the signals which terminate a program due to
       "external circumstances" as opposed to "internal bugs" *)
    Signal.hup;
    Signal.quit;
    Signal.term;
  ]

let getppid_exn pid =
  In_channel.read_lines ("/proc/" ^ Pid.to_string pid ^ "/status")
  |! List.find_exn ~f:(String.is_prefix ~prefix:"PPid:")
  |! String.split ~on:'\t'
  |! function
      | ["PPid:"; ppid] -> Pid.of_string ppid
      | _ -> failwithf "couldn't parse ppid from /proc/%s/status" (Pid.to_string pid) ()

let get_ppids pid =
  (* please indulge me *)
  let rec unfold ~init ~f =
    match f init with
    | Some value -> value :: unfold ~init:value ~f
    | None -> []
  in
  Option.try_with (fun () ->
    unfold ~init:pid ~f:(fun p ->
      if p = Pid.init then None else Some (getppid_exn p)))
