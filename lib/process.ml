open Core.Std
module Unix = Caml.UnixLabels
module Sys  = Caml.Sys

let rec temp_failure_retry f =
  try
    f ()
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    temp_failure_retry f


let close_non_intr fd =
  temp_failure_retry (fun () -> Unix.close fd)

(* Creates a unix pipe with both sides set close on exec *)
let cloexec_pipe () =
  let (fd1,fd2) as res = Unix.pipe () in
  Unix.set_close_on_exec fd1;
  Unix.set_close_on_exec fd2;
  res

module Process_info = struct
  type t = {
    pid:int;
    stdin : Unix.file_descr;
    stdout : Unix.file_descr;
    stderr : Unix.file_descr;
  }
end
(* We use a slightly more powerful version of create process than the one in
   core. This version is not quite as carefuly code reviewed but allows us to
   have more control over the forked side of the process (e.g.: chdir).
*)
let internal_create_process ?working_dir ?setuid ?setgid ~env ~prog ~args () =
  let close_on_err = ref [] in
  try
    let (in_read, in_write) = cloexec_pipe () in
    close_on_err := in_read :: in_write :: !close_on_err;
    let (out_read, out_write) = cloexec_pipe () in
    close_on_err := out_read :: out_write :: !close_on_err;
    let (err_read, err_write) = cloexec_pipe () in
    close_on_err := err_read :: err_write :: !close_on_err;
    let pid = Extended_unix.fork_exec
      prog
      args
      ?working_dir
      ?setuid
      ?setgid
      ~env
      ~stdin:in_read
      ~stdout:out_write
      ~stderr:err_write
    in
    close_non_intr in_read;
    close_non_intr out_write;
    close_non_intr err_write;
    {
      Process_info.pid = Pid.to_int pid;
      stdin = in_write;
      stdout = out_read;
      stderr = err_read
    }
  with e ->
    List.iter
      ~f:(fun fd -> try close_non_intr fd with _ -> ())
      !close_on_err;
    raise e


(**
   Remembers the last n-characters appended to it....
*)
module Tail_buffer = struct
  (** remembers the output in a circular buffer.
      looped is used to tell whether we loop around the
      boundary of the buffer.
  *)
  type t = {
    buffer : string;
    length : int;
    mutable looped : bool;
    mutable position : int;
  }

  let contents b =
    if not b.looped then
      String.sub b.buffer ~pos:0 ~len:b.position
    else
      let dst = String.create (b.length + 3) in
      dst.[0] <- '.';
      dst.[1] <- '.';
      dst.[2] <- '.';
      String.blit
        ~src:b.buffer
        ~dst ~dst_pos:3
        ~src_pos:b.position
        ~len:(b.length - b.position);
      String.blit ~src:b.buffer
        ~dst
        ~dst_pos:(b.length - b.position + 3)
        ~src_pos:0
        ~len:(b.position);
      dst

  let create len = {
    buffer = String.create len;
    length = len;
    looped = false;
    position = 0
  }

  let add b src len =
    if b.length <= len then begin
      String.blit
        ~src
        ~dst:b.buffer
        ~dst_pos:0
        ~src_pos:(len - b.length)
        ~len:(b.length);
      b.looped <- true;
      b.position <- 0
    end else
      let leftover =  b.length - b.position in
      if (len < leftover) then begin
        String.blit ~src ~dst:b.buffer ~dst_pos:b.position ~src_pos:0 ~len;
        b.position <- b.position + len;
      end else begin
        String.blit ~src ~dst:b.buffer ~dst_pos:b.position ~src_pos:0
          ~len:leftover;
        b.looped <- true;
        let len = (len-leftover) in
        String.blit ~src ~dst:b.buffer ~dst_pos:0 ~src_pos:leftover ~len;
        b.position <- len
      end
end

module Status = struct
  type t =
  [ `Timeout of Time.Span.t
  | `Exited of int
  | `Signaled of Signal.t
           (* WStopped is impossible*)
  ]
  with sexp_of

  let to_string = function
    | `Exited i -> sprintf "exited with code %d" i
    | `Signaled s ->
      sprintf "died after receiving %s (signal number %d)"
        (Signal.to_string s) (Signal.to_system_int s)
    | `Timeout s -> sprintf "Timed out (ran for %s)" (Time.Span.to_string s)

end
module Command_result = struct
  type t= {
    status: Status.t;
    stdout_tail : string;
    stderr_tail : string
  }
end

(** wait for a given pid to exit;
    returns true when the process exits and false if the process is still runing
    after waiting for [span]
*)
let wait_for_exit ?(is_child=false) span pid =
  let end_time = Time.add (Time.now ()) span in
  let exited () =
    if is_child then begin
      (* Non interuptible when used with WNOHANG*)
      match Caml.Unix.waitpid [Caml.Unix.WNOHANG] pid with
      | 0,_  -> true
      | v, _ -> assert (v=pid); false
    end else
      try
        (* The conversion function for signals is the identity on 0 so this is
           the equivalent of calling the c kill with 0 (test whether a process
           exists) *)
        Caml.Unix.kill pid 0; (* Non interuptible *)
        true
      with Unix.Unix_error (Unix.ESRCH,_,_) -> false
  in
  let rec loop () =
    if Time.(>) (Time.now ()) end_time then
      false
        (*We need to explicitely waitpid the child otherwise we are sending
          signals to a zombie*)
    else if not (exited ()) then true
    else begin
      Time.pause (sec 0.1);
      loop ()
    end
  in
  loop ()

let kill
    ?is_child
    ?(wait_for=sec 2.0)
    ?(signal=Caml.Sys.sigterm)
    pid
    =
  Caml.Unix.kill pid signal;
  if not (wait_for_exit ?is_child wait_for pid) then begin
    begin
      try
        Caml.Unix.kill Caml.Sys.sigkill pid
      with Unix.Unix_error (Unix.ESRCH,_,_) -> ()
    end;
    if not (wait_for_exit wait_for pid) then begin
      failwithf "Process.kill failed to kill %i \
             (or the process wasn't collected by its parent)"
        pid
        ()
    end
  end


type t = {
  mutable open_fds : Unix.file_descr list;
  mutable in_fds   : Unix.file_descr list;
  mutable out_fds  : Unix.file_descr list;
  keep_open        : bool;
  buf              : String.t;
  in_cnt           : String.t;
  in_len           : int;
  out_callbacks    : (Unix.file_descr*(string -> int -> unit)) list;
  pid              : int;
  mutable in_pos   : int;
}

let close_pooled state fd =
  if List.mem state.open_fds fd then
    close_non_intr fd;
  state.open_fds <- List.filter ~f:((<>) fd) state.open_fds;
  state.out_fds  <- List.filter ~f:((<>) fd) state.out_fds;
  state.in_fds   <- List.filter ~f:((<>) fd) state.in_fds

let process_io ~read ~write state =
  List.iter write
    ~f:(fun fd ->
          (try
             let len =
               temp_failure_retry (fun () ->
                 Unix.single_write fd
                   ~buf:state.in_cnt
                   ~pos:state.in_pos
                   ~len:(state.in_len - state.in_pos))
             in
             state.in_pos <- state.in_pos + len;
             (* Close the process's in_channel iff we are done writing to it*)
             if len = 0 then
               if state.keep_open then
                 state.in_fds <- List.filter ~f:((<>) fd) state.in_fds
               else
                 close_pooled state fd
           with Unix.Unix_error (Unix.EPIPE,_,_) ->
             close_pooled state fd));
  List.iter read
    ~f:(fun fd ->
      let len =
        temp_failure_retry
          (fun () -> Unix.read fd
            ~buf:state.buf
            ~pos:0
            ~len:(String.length state.buf))
      in
      if len = 0 then
        close_pooled state fd
      else
        let callback = List.Assoc.find_exn state.out_callbacks fd in
        callback state.buf len)

let available_fds state ~timeout =
  let read,write,_conds =
    temp_failure_retry (fun () ->
      Unix.select
        ~read:state.out_fds
        ~write:state.in_fds
        ~except:[]
        ~timeout)
  in
  read,write

let create
    ~keep_open
    ~use_extra_path
    ~working_dir
    ~setuid
    ~setgid
    ~prog
    ~args
    ~stdoutf
    ~stderrf
    ~input_string
    ~env =
  let full_prog = Shell__core.path_expand ?use_extra_path prog in
  let process_info =
    internal_create_process
      ?working_dir ?setuid ?setgid
      ~env ~prog:full_prog ~args ()
  in

  let out_fd = process_info.Process_info.stdout
  and in_fd = process_info.Process_info.stdin
  and err_fd = process_info.Process_info.stderr
  and pid = process_info.Process_info.pid in

  {
    keep_open;
    open_fds = [in_fd;out_fd;err_fd];
    in_fds   = [in_fd];
    out_fds  = [err_fd;out_fd];
    buf      = String.create 4096;
    in_cnt   = input_string;
    in_pos   = 0;
    in_len   = String.length input_string;
    out_callbacks = [out_fd,stdoutf;
                     err_fd,stderrf];
    pid
  }

let rec finish_reading state =
  match available_fds state ~timeout:0. with
  | []  ,_ -> ()
  | read,_ ->
    process_io state ~read ~write:[];
    finish_reading state

let rec run_loop ~start_time ~timeout state =
  let read,write = available_fds state ~timeout:0.1 in
  begin
    try
      process_io state ~read ~write
    with e ->
      kill ~is_child:true state.pid;
      raise e
  end;
  let elapsed = Time.diff (Time.now ()) start_time in
  match timeout with
  | Some timeout when Time.Span.(elapsed > timeout) ->
    kill ~is_child:true state.pid;
    finish_reading state;
    `Timeout elapsed
  | None | Some _ ->
    match Caml.Unix.waitpid [Caml.Unix.WNOHANG] state.pid with
    | 0,_ -> run_loop ~start_time ~timeout state
    | _, status ->
      finish_reading state;
      match status with
      | Caml.Unix.WEXITED i   -> `Exited i
      | Caml.Unix.WSIGNALED s -> `Signaled (Signal.of_caml_int s)
      | Caml.Unix.WSTOPPED _  -> assert false


let run
    ?timeout
    ?use_extra_path
    ?working_dir
    ?setuid
    ?setgid
    ?(env=`Extend [])
    ?input:(input_string="")
    ?(keep_open=false)
    ?(stdoutf=(fun _string _len -> ()))
    ?(stderrf=(fun _string _len -> ()))
    ?(tail_len = 2048) ~prog ~args
    () =
  let stdout_tail = Tail_buffer.create tail_len
  and stderr_tail = Tail_buffer.create tail_len in
  let stdoutf sbuf len =
    stdoutf sbuf len;
    Tail_buffer.add stdout_tail sbuf len
  and stderrf sbuf len =
    stderrf sbuf len;
    Tail_buffer.add stderr_tail sbuf len
  in
  let status =
    protectx (Sys.signal Sys.sigpipe Sys.Signal_ignore,
              create
                ~keep_open
                ~use_extra_path
                ~working_dir
                ~setuid
                ~setgid
                ~stderrf
                ~stdoutf
                ~prog
                ~args
                ~env
                ~input_string)
      ~f:(fun (_old_sigpipe,state) ->
            run_loop state
              ~start_time:(Time.now ())
              ~timeout;)
      ~finally:(fun (old_sigpipe,state) ->
        List.iter state.open_fds
          ~f:close_non_intr;
        ignore (Sys.signal Sys.sigpipe old_sigpipe : Sys.signal_behavior))
  in
  {Command_result.
     status      = status;
     stdout_tail = Tail_buffer.contents stdout_tail;
     stderr_tail = Tail_buffer.contents stderr_tail }

(* Externally export this *)
let kill ?is_child ?wait_for ?signal pid =
  let signal = Option.map ~f:Signal.to_caml_int signal in
  kill
    ?is_child
    ?wait_for
    ?signal
    (Pid.to_int pid)
