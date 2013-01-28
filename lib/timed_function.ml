open Core.Std

exception Timeout

type 'a forked_computation = ('a,Sexp.t) Result.t with sexp

let run_in_fork ~f ~sexp_of v =
  let pipe_read,pipe_write = Unix.pipe () in
  match Unix.fork() with
  | `In_the_child ->
      Unix.close pipe_read;
      let oc = Unix.out_channel_of_descr pipe_write in
      let res =
        try
          Ok (f v)
        with e -> Error (Exn.sexp_of_t e)
      in
      Sexp.output oc (sexp_of_forked_computation sexp_of res);
      exit 0
  | `In_the_parent pid ->
      Unix.close pipe_write;
      pid,pipe_read

(** All our input comes in one burst so we do not need to run a select loop... *)
let wait_for_input ~timeout fd =
  let select_fds =
    Unix.select ()
      ~restart:true
      ~timeout
      ~read:[fd]
      ~write:[]
      ~except:[]
  in
  if select_fds.Unix.Select_fds.read = [] then
    None
  else
    Some (In_channel.input_all (Unix.in_channel_of_descr fd))

let run ~timeout ~f ~sexp_of ~of_sexp v =
  let pid,pipe_read = run_in_fork ~f ~sexp_of v in
  protectx ()
    ~f:(fun () ->
          match wait_for_input ~timeout pipe_read with
          | None ->
              (* We timed out *)
              Process.kill ~is_child:true pid;
              raise Timeout
          | Some s ->
              let status = snd (Unix.wait (`Pid pid) ~restart:true) in
              if Result.is_error status then begin
                failwithf "Timed forked-out process exited with status %s"
                  (Unix.Exit_or_signal.to_string_hum status)
                  ()
              end;
              match forked_computation_of_sexp of_sexp (Sexp.of_string s) with
              | Result.Error e ->
                  failwithf "Timed forked-out function died with exception %s"
                    (Sexp.to_string_hum e)
                    ();
              | Result.Ok ok -> ok)
    ~finally:(fun () -> Unix.close pipe_read)
