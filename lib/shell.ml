(* TODO: Ron wants the ability to run interactive commands and to expose the fd
   version of process handling.*)
open Core.Std

module Line_buffer = Shell__line_buffer

include (Shell__core:sig val extra_path : string list ref end)

module Process = struct

  exception Early_exit with sexp

  (* Same thing as Signal.t but actually has a somewhat human readable output...
  *)
  type signal = Signal.t
  let sexp_of_signal s = Sexp.Atom (Signal.to_string s)

  type status = [ `Timeout of Time.Span.t | Process.Status.t ] with sexp_of
(*  type status = (unit, error) Result.t with sexp_of *)

  type t = {
      program   : string;
      arguments : string list;
  } with sexp_of

  type result = {
    command : t;
    status  : status;
    stdout  : string;
    stderr  : string;
  } with sexp_of

  exception Failed of result with sexp

  let to_string {program=prog; arguments=args} =
    let f s =
      if not (String.contains s ' ') &&
        not (String.contains s '"') then
        s
      else
        sprintf "%S" s
    in
    String.concat ~sep:" " (List.map ~f (prog::args))

  let status_to_string = function
    | `Timeout t -> sprintf "Timed out (ran for %s)" (Time.Span.to_string t)
    | #Process.Status.t as s -> Process.Status.to_string s

  let format_failed c =
    String.concat ~sep:" " ["Command failed:";
                            to_string c.command;
                            "Exit status:";
                            status_to_string c.status;
                            "stderr:";
                            c.stderr]

  let () = Caml.Printexc.register_printer (function
    | Failed r -> Some (format_failed r)
    | _ -> None)

  module Defaults = struct
    let timeout = ref None
    let verbose = ref false
    let echo = ref false
  end

  let set_defaults ?timeout ?verbose ?echo () =
    Option.iter ~f:(fun v -> Defaults.verbose := v) verbose;
    Option.iter ~f:(fun v -> Defaults.timeout := v) timeout;
    Option.iter ~f:(fun v -> Defaults.echo := v)    echo


  let cmd program arguments = {
    program   = program;
    arguments = arguments;
  }

  let shell s =
    {
      program   = "/bin/bash";
      arguments = [ "-c" ; s ]
    }

  (* avoid asking for the password at all costs. *)
  let noninteractive_ssh_options = ["-o";"BatchMode yes"]
  let noninteractive_no_hostkey_checking_options = [
    "-n"; "-q"; "-x";
    "-o"; "ConnectTimeout=10";
    "-o"; "CheckHostIP=no";
    "-o"; "StrictHostKeyChecking=no";
    "-o"; "BatchMode=yes";
  ]

  (* Passes the remote command to ssh *)
  (* quote_args quotes all arguments to the shell.  We need to escape all the
     arguments because ssh is passing this to the remote shell which will
     unescape all of that before passing it over to our program.*)
  let remote ?(ssh_options = noninteractive_ssh_options) ?(quote_args=true)
      ?user ~host cmd   =
    let url = match user with
      | None      -> host
      | Some user -> user ^"@"^host
    in
    let args = cmd.program :: cmd.arguments in
    let args =
      if quote_args then List.map ~f:Filename.quote args
      else args
    in
    { program   = "/usr/bin/ssh";
      arguments = ssh_options @ [url] @ args;
    }

  type 'res acc =
      { add           : string -> int -> [`Stop | `Continue];
        flush         : unit -> 'res; }

  type 'res reader = unit -> 'res acc

  let run_k' k
      ?use_extra_path
      ?(timeout = !Defaults.timeout)
      ?working_dir ?setuid ?setgid ?env
      ?(verbose = !Defaults.verbose)
      ?(echo = !Defaults.echo)
      ?input
      ?keep_open
      =
    k (fun cmd stdoutf ->
      if echo then
        Console.Ansi.printf [`Underscore] "Shell: %s\n%!" (to_string cmd);
      let stderrf =
        if verbose then
          (fun s len -> Console.Ansi.output [`Red] stderr s 0 len)
        else
          (fun _s _len -> ())
      and stdoutf =
        if verbose then
          (fun s len ->
            Console.Ansi.output [`Green] stderr s 0 len;
            stdoutf s len)
        else stdoutf
      in
      (Process.run
         ?timeout ?input ?keep_open ?working_dir
         ?setuid ?setgid ?use_extra_path ?env
         ~stdoutf
         ~stderrf
         ~prog:cmd.program
         ~args:cmd.arguments ()))

  let run_k k ?(expect = [0]) = run_k' (fun f ->
    k (fun cmd reader ->
      let acc = reader () in
      let stdoutf s len =
        match acc.add s len with
        | `Continue -> ()
        | `Stop -> raise Early_exit
      in
     try
        let r = f cmd stdoutf in
        let module Res = Process.Command_result in
        match r.Res.status with
        | `Exited i when List.mem expect i -> acc.flush ()
        | status ->
          raise (Failed
                   { command = cmd;
                     status  = (status :> status);
                     stderr  = r.Res.stderr_tail;
                     stdout  = r.Res.stdout_tail;
                   })
      with Early_exit -> acc.flush ()))


  let run ?expect = run_k (fun f cmd reader -> f cmd reader) ?expect

  let test_k k ?(true_v = [0]) ?(false_v = [1]) = run_k' (fun f ->
    k (fun cmd ->
      let r = f cmd (fun _ _ -> ()) in
      let module Res = Process.Command_result in
      match r.Res.status with
      | `Exited i when List.mem true_v i -> true
      | `Exited i when List.mem false_v i -> false
      | #status as status ->
        raise (Failed
                 { command = cmd;
                   status  = (status :> status);
                   stderr  = r.Res.stderr_tail;
                   stdout  = r.Res.stdout_tail })))

  let test ?true_v = test_k (fun f cmd -> f cmd) ?true_v

  let discard () = { add   = (fun _ _ -> `Continue); flush = (fun () -> ()) }

  let callback ~add ~flush () =
    { add = (fun s len -> add s len;`Continue); flush }

  let content () =
    let buffer = Buffer.create 16 in
    {
      add   = (fun s len -> Buffer.add_substring buffer s 0 len; `Continue);
      flush = (fun () -> Buffer.contents buffer);
    }

  let fold_lines (type ret) (type v)
      ?eol
      ~(init:v)
      ~(f: v -> string -> (v * [`Continue | `Stop]))
      ~(flush:v -> ret)
      () : ret acc
      =
    let acc = ref init
    and continue = ref `Continue in
    let lb = Line_buffer.create
      ?eol
      (fun line ->
        let acc_v,continue_v = f !acc line in
        acc := acc_v;
        continue := continue_v)
    in
    { add =
        (fun s len ->
          Line_buffer.add_substring lb s ~pos:0 ~len;
          !continue);
      flush = (fun () ->
        if !continue = `Continue then
          Line_buffer.flush lb;
        flush !acc) }

  let lines ?eol () =
    fold_lines
      ?eol
      ~flush:List.rev
      ~init:[]
      ~f:(fun acc line -> (line::acc) , `Continue)

  let head ?eol () = fold_lines
    ?eol
    ~flush:(function Some v -> v | None -> assert false)
    ~init:None
    ~f:(fun _acc line -> Some line,`Stop)

end

type 'a with_process_flags =
  ?use_extra_path:bool
  -> ?timeout:Time.Span.t option
  -> ?working_dir:string (* rename to run_in? *)
  -> ?setuid:int
  -> ?setgid:int
  -> ?env:[`Extend of (string * string) list
          |`Replace of (string * string) list]
  -> ?verbose:bool
  -> ?echo:bool
  -> ?input:string
  -> ?keep_open:bool
  -> 'a

type 'a with_run_flags =
     (* Defaults to [0]*)
    ?expect:int list -> ('a with_process_flags)

type 'a with_test_flags =
  ?true_v:int list -> ?false_v:int list -> ('a with_process_flags)

type 'a cmd = string -> string list -> 'a

type ('a,'ret) sh_cmd = (('a, unit, string,'ret) format4 -> 'a)

let run_gen reader =
  Process.run_k (fun f prog args -> f (Process.cmd prog args) reader)

let run = run_gen Process.discard
let run_lines ?eol = run_gen (Process.lines ?eol ())
let run_one ?eol = run_gen (Process.head ?eol ())
let run_full = run_gen Process.content
let run_fold ?eol ~init ~f = run_gen
  (Process.fold_lines ?eol ~init ~f ~flush:(fun x -> x))
(*
TEST_UNIT =
  (* This should not hand because the stdin is closed... *)
  run ~timeout:(Some (sec 0.5)) "cat" []
TEST_UNIT =
  try
    run ~timeout:(Some (sec 0.5)) "cat" []
  with Process.
*)

let test =
  Process.test_k (fun f prog args -> f (Process.cmd prog args))

let k_shell_command k f fmt =
  ksprintf (fun command -> k f (Process.shell command)) fmt

let sh_gen reader=
  Process.run_k (k_shell_command (fun f cmd -> f cmd reader))

let sh       ?expect = sh_gen Process.discard ?expect
let sh_one   ?expect = sh_gen (Process.head ())   ?expect
let sh_lines ?expect = sh_gen (Process.lines ())  ?expect
let sh_full  ?expect = sh_gen Process.content ?expect

TEST =
  sh_lines "yes yes | head -n 200000" =
   List.init 200_000 ~f:(fun _num -> "yes")

let sh_test ?true_v =
  Process.test_k (k_shell_command (fun f cmd -> f cmd)) ?true_v

type 'a with_ssh_flags =
  ?ssh_options:string list -> ?user:string -> host:string -> 'a

let noninteractive_ssh_options = Process.noninteractive_ssh_options
let noninteractive_no_hostkey_checking_options =
  Process.noninteractive_no_hostkey_checking_options

let k_remote_command k ?ssh_options ?quote_args ?user ~host =
  k_shell_command (fun f cmd ->
    k f (Process.remote ?ssh_options ?quote_args ?user ~host cmd))

let ssh_gen reader ?ssh_options ?user ~host =
  Process.run_k (k_remote_command (fun f cmd -> f cmd reader)
                   ?ssh_options ~quote_args:true ?user ~host)

let ssh       ?ssh_options = ssh_gen Process.discard ?ssh_options
let ssh_one   ?ssh_options = ssh_gen (Process.head ()) ?ssh_options
let ssh_lines ?ssh_options = ssh_gen (Process.lines ()) ?ssh_options
let ssh_full  ?ssh_options = ssh_gen Process.content ?ssh_options

let ssh_test ?ssh_options ?user ~host =
  Process.test_k (k_remote_command (fun f cmd -> f cmd)
                    ?ssh_options ~quote_args:true ?user ~host)

let whoami = Shell__core.whoami
let which = Shell__core.which

let ln ?s ?f src dst =
  let s = Option.map s ~f:(fun () -> "-s") in
  let f = Option.map f ~f:(fun () -> "-f") in
  run "/bin/ln" (List.filter_map ~f:ident [s; f] @ ["-n"; "--"; src; dst])

let rm ?r ?f path =
  let r = Option.map r ~f:(fun () -> "-r") in
  let f = Option.map f ~f:(fun () -> "-f") in
  run "/bin/rm" (List.filter_map ~f:ident [r; f; Some "--"; Some path])

let mv src dst =
  run "/bin/mv" ["--";src;dst]

let mkdir ?p ?perm path =
  let p = Option.map p ~f:(fun () -> "-p") in
  let mode = Option.map perm ~f:(sprintf "--mode=%o") in
  run "/bin/mkdir" (List.filter_map ~f:ident [p; mode;Some "--";Some path])

(* TODO: Deal with atomicity  *)
let cp ?(overwrite=true) ?perm src dst =
  let perm = match perm with
    | Some p -> p
    | None -> (Unix.lstat src).Unix.st_perm
  in
  let dst = if Sys.is_directory dst = `Yes then
      dst ^/ (Filename.basename src)
    else
      dst
  in
  let out_mode =
    if overwrite then
      [ Unix.O_WRONLY; Unix.O_NOCTTY; Unix.O_CREAT; Unix.O_TRUNC ]
    else
      [ Unix.O_WRONLY; Unix.O_NOCTTY; Unix.O_CREAT; Unix.O_EXCL ]
  in
  protectx (Unix.openfile src ~mode:[ Unix.O_RDONLY; Unix.O_NOCTTY ] ~perm:0)
    ~f:(fun infh ->
          protectx (Unix.openfile dst ~mode:out_mode ~perm)
            ~f:(fun outfh ->
                  let buflen = 4096 in
                  let buf = String.create buflen in
                  let rec loop () =
                    let rlen = Unix.read infh ~buf ~pos:0 ~len:buflen in
                    if rlen <> 0 then
                      let wlen = Unix.write outfh ~buf ~pos:0 ~len:rlen in
                      if rlen <> wlen then
                        failwithf "Short write: tried to write %d bytes, \
                                   only wrote %d bytes" rlen wlen ();
                      loop ()
                  in
                  loop ();
               )
            ~finally:Unix.close
       )
    ~finally:Unix.close
;;

let scp ?(compress=false) ?(recurse=false) ?user ~host f t =
  let user_arg = Option.value_map user ~default:"" ~f:(fun user -> user ^ "@") in
  let args = [f; user_arg ^ host ^ ":" ^ t] in
  let args = if recurse then "-r"::args else args in
  let args = if compress then "-C"::args else args in
  run "scp" args
;;
