open Core.Std
open Core_extended.Std

module type T = sig
  type main_fun
  val lock_file : string
  val name : string
  val spec : (main_fun, unit) Fcommand.t
  val main : main_fun
end

type t = (module T)

let create (type a) ~lock_file ~name spec main =
  let module T =
    struct
      type main_fun = a
      let lock_file = lock_file
      let name = name
      let spec = spec
      let main = main
    end
  in
  (module T : T)

let lock_file t = let module T = (val t : T) in T.lock_file
let name      t = let module T = (val t : T) in T.name

let start_daemon ~lock_file main =
  let release_parent = Daemon.daemonize_wait () in
  (* lock file created after [daemonize_wait] so that *child* pid is written
      to the lock file rather than the parent pid *)
  if Lock_file.create ~close_on_exec:false lock_file
    (* this writes our pid in the file *)
  then begin
    (* we release the daemon's parent *after* the lock file is created
       so that any error messages during lock file creation happen
       prior to severing the daemon's connection to std{out,err} *)
    release_parent ();
    (* unix automatically handles unlocking the lock file, but it is
       our responsibility to delete it *)
    at_exit (fun () -> Unix.unlink lock_file);
    main
  end else begin
    prerr_endline "lock file already held. refusing to start";
    exit 1
  end

let check_lock_file t =
  let lock_file = lock_file t in
  if Lock_file.is_locked lock_file then begin
    let pid = Pid.t_of_sexp (Sexp.load_sexp lock_file) in
    `Running_with_pid pid
  end else
    `Not_running

let still_alive pid =
  (* receiving [Signal.zero] is a no-op, but sending it gives info
     about whether there a running process with that pid *)
  match Signal.send Signal.zero (`Pid pid) with
  | `Ok -> true
  | `No_such_process -> false

let stop_signal_flag () =
  let open Fcommand in
  flag "-kill" ~doc:" send SIGKILL instead of SIGTERM"
    Flag.(map no_arg ~f:(function
      | `Present -> Signal.kill
      | `Absent -> Signal.term))

let status_command t =
  Fcommand.cmd ~summary:(sprintf "check status of %s" (name t))
    Fcommand.(anon zero)
    (fun () ->
      match check_lock_file t with
      | `Not_running -> printf "%s is not running\n%!" (name t)
      | `Running_with_pid pid ->
        if still_alive pid then
          printf "%s is running with pid %s\n%!" (name t) (Pid.to_string pid)
        else
          printf "%s is not running, even though we saw pid %s in its lockfile\n%!"
            (name t) (Pid.to_string pid))

let stop signal t =
  let was_not_running () =
    eprintf "%s was not running\n%!" (name t);
    `Was_not_running
  in
  match check_lock_file t with
  | `Not_running -> was_not_running ()
  | `Running_with_pid pid ->
    let timeout_span = sec 10. in
    let deadline = Time.add (Time.now ()) timeout_span in
    match Signal.send signal (`Pid pid) with
    | `No_such_process -> was_not_running ()
    | `Ok ->
      let rec wait_loop () =
        if Time.(>=) (Time.now ()) deadline then begin
          eprintf "failed to observe %s die after %s\n%!" (name t)
            (Time.Span.to_string timeout_span);
          `Did_not_die
        end else if still_alive pid then begin
          Time.pause (sec 0.2);
          wait_loop ()
        end else
          `Died
      in
      wait_loop ()

let stop_command t =
  let module T = (val t : T) in
  Fcommand.cmd ~summary:(sprintf "stop %s" T.name)
    (stop_signal_flag ())
    (fun signal ->
      match stop signal t with
      | `Was_not_running | `Did_not_die -> exit 1
      | `Died -> exit 0)

let start_command t =
  let module T = (val t : T) in
  Fcommand.cmd ~summary:(sprintf "restart %s" T.name)
    Fcommand.(const () ++ T.spec)
    (fun () -> start_daemon ~lock_file:T.lock_file T.main)

let restart_command t =
  let module T = (val t : T) in
  Fcommand.cmd ~summary:(sprintf "restart %s" T.name)
    Fcommand.(stop_signal_flag () ++ T.spec)
    (fun signal ->
      match stop signal t with
      | `Did_not_die -> exit 1
      | `Was_not_running | `Died -> start_daemon ~lock_file:T.lock_file T.main)

let group t =
  Command.group ~summary:(sprintf "manage %s" (name t)) [
    ("start",   start_command   t);
    ("stop",    stop_command    t);
    ("restart", restart_command t);
    ("status",  status_command  t);
  ]

let start   = start_command
let stop    = stop_command
let restart = restart_command
let status  = status_command

