open Core.Std

type slot = {
  lock_file : string;
  name : string;
  redirect_stdout : Daemon.redirect_fds;
  redirect_stderr : Daemon.redirect_fds;
}

module type T = sig
  type main
  val slot_spec : unit -> (slot -> 'm, 'm) Core_command.Spec.t
  val main_spec : (foreground:bool -> main, unit) Core_command.Spec.t
  val main : slot -> main
end

type t = (module T)

let start_daemon slot main ~foreground =
  let release_parent =
    if foreground then
      Fn.id
    else
      Daemon.daemonize_wait ()
        ~redirect_stdout:slot.redirect_stdout
        ~redirect_stderr:slot.redirect_stderr
  in
  (* lock file created after [daemonize_wait] so that *child* pid is written
      to the lock file rather than the parent pid *)
  if Lock_file.create ~close_on_exec:false slot.lock_file
    (* this writes our pid in the file *)
  then begin
    (* we release the daemon's parent *after* the lock file is created
      so that any error messages during lock file creation happen
      prior to severing the daemon's connection to std{out,err} *)
    release_parent ();
    (* unix automatically handles unlocking the lock file, but it is
      our responsibility to delete it *)
    at_exit (fun () ->
      print_endline "removing lockfile";
      Unix.unlink slot.lock_file);
    main slot
  end else begin
    eprintf "lock file already held for %s. refusing to start.\n%!" slot.name;
    exit 1
  end

let check_lock_file {lock_file; _} =
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
  Core_command.Spec.(
    step (fun m kill -> m (if kill then Signal.kill else Signal.term))
    ++ flag "-kill" no_arg ~doc:" send SIGKILL instead of SIGTERM"
  )

let stop signal slot =
  let was_not_running () =
    eprintf "%s was not running\n%!" slot.name;
    `Was_not_running
  in
  match check_lock_file slot with
  | `Not_running -> was_not_running ()
  | `Running_with_pid pid ->
    let timeout_span = sec 10. in
    let deadline = Time.add (Time.now ()) timeout_span in
    match Signal.send signal (`Pid pid) with
    | `No_such_process -> was_not_running ()
    | `Ok ->
      let rec wait_loop () =
        if Time.(>=) (Time.now ()) deadline then begin
          eprintf "failed to observe %s die after %s\n%!" slot.name
            (Time.Span.to_string timeout_span);
          `Did_not_die
        end else if still_alive pid then begin
          Time.pause (sec 0.2);
          wait_loop ()
        end else
          `Died
      in
      wait_loop ()

let status_command t =
  let module T = (val t : T) in let () = () in
  Core_command.basic ~summary:(sprintf "check status of daemon")
    Core_command.Spec.(const () ++ T.slot_spec ())
    (fun () slot ->
      match check_lock_file slot with
      | `Not_running -> printf "%s is not running\n%!" slot.name
      | `Running_with_pid pid ->
        if still_alive pid then
          printf "%s is running with pid %s\n%!" slot.name (Pid.to_string pid)
        else
          printf "%s is not running, even though we saw pid %s in its lockfile\n%!"
            slot.name
            (Pid.to_string pid))

let stop_command t =
  let module T = (val t : T) in let () = () in
  Core_command.basic ~summary:"stop daemon"
    Core_command.Spec.(stop_signal_flag () ++ T.slot_spec ())
    (fun signal slot ->
      match stop signal slot with
      | `Was_not_running | `Did_not_die -> exit 1
      | `Died -> exit 0)

let start_command t =
  let module T = (val t : T) in let () = () in
  Core_command.basic ~summary:"restart daemon"
    Core_command.Spec.(const () ++ T.slot_spec () ++ T.main_spec)
    (fun () slot -> start_daemon slot T.main)

let restart_command t =
  let module T = (val t : T) in let () = () in
  Core_command.basic ~summary:"restart daemon"
    Core_command.Spec.(stop_signal_flag () ++ T.slot_spec () ++ T.main_spec)
    (fun signal slot ->
      match stop signal slot with
      | `Did_not_die -> exit 1
      | `Was_not_running | `Died -> start_daemon slot T.main)

let group t ~summary =
  Core_command.group ~summary [
    ("start",   start_command   t);
    ("stop",    stop_command    t);
    ("restart", restart_command t);
    ("status",  status_command  t);
  ]

let start   = start_command
let stop    = stop_command
let restart = restart_command
let status  = status_command

