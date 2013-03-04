open Core.Std

(* Useful debugging functions working at a low level. *)

external stop_upon_sigbus : unit -> unit = "low_level_debug_stop_upon_sigbus"
external stop_upon_sigsegv : unit -> unit = "low_level_debug_stop_upon_sigsegv"
external stop_upon_sigpipe : unit -> unit = "low_level_debug_stop_upon_sigpipe"
external stop_upon_exit : unit -> unit = "low_level_debug_stop_upon_exit"

external stop_me_now : unit -> unit = "low_level_debug_stop_me_now"

let segfault_me_now () =
  let string_that_should_be_easy_to_detect =
    "The major difference between a thing that might go wrong and a thing that \
     cannot possibly go wrong is that when a thing that cannot possibly go wrong \
     goes wrong it usually turns out to be impossible to get at or repair."
  in
  Obj.set_field (Obj.repr 0) 0 (Obj.repr string_that_should_be_easy_to_detect)

external start_canary_thread_internal :
  max_wait:float -> check_interval:int -> never_returns = "start_canary"

let start_canary_thread =
  let started = ref false in
  let lock = Mutex.create () in
  fun ~max_wait ~check_interval ->
    Mutex.lock lock;
    if !started then failwith "canary thread already started, one allowed per process";
    started := true;
    Mutex.unlock lock;
    let check_interval = Time.Span.to_sec check_interval in
    if check_interval >= 1. then invalid_arg "check_interval must be < 1s";
    let check_interval = Float.iround_exn ~dir:`Zero (check_interval *. 1_000_000.) in
    let (_ : Thread.t) =
      Thread.create (fun () ->
        start_canary_thread_internal
          ~max_wait:(Time.Span.to_sec max_wait) ~check_interval)
        ()
    in
    ()
;;

(* It seems that veneers like these are needed so that the functions are
   correctly exported in the object files. *)
let stop_upon_sigbus () = stop_upon_sigbus ()
let stop_upon_sigsegv () = stop_upon_sigsegv ()
let stop_upon_sigpipe () = stop_upon_sigpipe ()
let stop_upon_exit () = stop_upon_exit ()
let stop_me_now () = stop_me_now ()
