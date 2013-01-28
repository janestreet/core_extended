open Core.Std

let command ~lock_file ~name main =
  let start_main () =
    let release_parent = Daemon.daemonize_wait () in
    (* lock file created after [daemonize_wait] so that *child* pid is written
       to the lock file rather than the parent pid *)
    if Lock_file.create ~close_on_exec:true ~unlink_on_exit:true lock_file
      (* this writes our pid in the file *) then begin
      (* we release the daemon's parent *after* the lock file is created
         so that any error messages during lock file creation happen
         prior to severing the daemon's connection to std{out,err} *)
      release_parent ();
      main ()
    end;
    0
  in
  let check_lock_file () =
    if Lock_file.is_locked lock_file then begin
      let pid = Pid.t_of_sexp (Sexp.load_sexp lock_file) in
      `Running_with_pid pid
    end else
      `Not_running
  in
  let still_alive pid =
    (* receiving [Signal.zero] is a no-op, but sending it gives info
       about whether there a running process with that pid *)
    match Signal.send Signal.zero (`Pid pid) with
    | `Ok -> true
    | `No_such_process -> false
  in
  let status_main () =
    begin match check_lock_file () with
    | `Not_running -> printf "%s is not running\n%!" name
    | `Running_with_pid pid ->
      if still_alive pid then
        printf "%s is running with pid %s\n%!" name (Pid.to_string pid)
      else
        printf "%s is not running, even though we saw pid %s in its lockfile\n%!"
          name (Pid.to_string pid)
    end;
    0
  in
  let stop_aux ~stop_signal =
    let was_not_running () =
      eprintf "%s was not running\n%!" name;
      `Was_not_running
    in
    match check_lock_file () with
    | `Not_running -> was_not_running ()
    | `Running_with_pid pid ->
      let timeout_span = sec 10. in
      let deadline = Time.add (Time.now ()) timeout_span in
      match Signal.send stop_signal (`Pid pid) with
      | `No_such_process -> was_not_running ()
      | `Ok ->
        let rec wait_loop () =
          if Time.(>=) (Time.now ()) deadline then begin
            eprintf "failed to observe %s die after %s\n%!" name
              (Time.Span.to_string timeout_span);
            `Did_not_die
          end else if still_alive pid then begin
            Time.pause (sec 0.2);
            wait_loop ()
          end else
            `Died
        in
        wait_loop ()
  in
  let stop_main ~stop_signal =
    match stop_aux ~stop_signal with
    | `Was_not_running | `Did_not_die -> 1
    | `Died -> 0
  in
  let restart_main ~stop_signal =
    match stop_aux ~stop_signal with
    | `Did_not_die -> 1
    | `Was_not_running | `Died -> start_main ()
  in
  let summary ~verb = sprintf "%s %s" verb name in
  let assert_no_anons anons =
    match anons with
    | [] -> ()
    | anons ->
      failwithf "expected 0 anonymous arguments but found %d\n%!"
        (List.length anons) ()
  in
  let base_cmd ~verb main =
    Command.create (fun () -> exit (main ()))
      ~summary:(summary ~verb)
      ~usage_arg:""
      ~init:Fn.id
      ~flags:[]
      ~final:(fun () anons -> assert_no_anons anons)
  in
  let stop_cmd ~verb main =
    Command.create (fun stop_signal -> exit (main ~stop_signal))
      ~summary:(summary ~verb)
      ~usage_arg:"[-kill]"
      ~init:(Fn.const Signal.term)
      ~flags:[
        Command.Flag.noarg_acc "-kill" (Fn.const Signal.kill)
          ~doc:" send SIGKILL instead of SIGTERM"
      ]
      ~final:(fun signal anons -> assert_no_anons anons; signal)
  in
  Command.group ~summary:(summary ~verb:"manage") [
    ("start",   base_cmd ~verb:"start" start_main);
    ("stop",    stop_cmd ~verb:"stop" stop_main);
    ("restart", stop_cmd ~verb:"restart" restart_main);
    ("status",  base_cmd ~verb:"check status of" status_main);
  ]
