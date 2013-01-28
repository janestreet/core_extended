(**
   Low-level process handling

   This is low-level enough that you should probably be using [Shell] instead
   to dispatch processes.
*)

open Core.Std

module Status : sig
  type t =   [ `Timeout of Time.Span.t
  | `Exited of int
  | `Signaled of Signal.t
  (* WStopped is impossible*)
  ]
  with sexp_of

  val to_string : t -> string

end

module Command_result : sig
  type t= {
    status      : Status.t;
    stdout_tail : string;
    stderr_tail : string
  }
end

(** kills a process by sending [signal]; waiting for [wait_for] and then
    sending a [sigkill].
    You need to set is_child to true when killing child processes or run waitpid
    on them in another.
    @raises Failure if the target program hangs for more that [wait_for] after
    receiving the [sigkill].
*)
val kill :
  ?is_child:bool ->
  ?wait_for:Time.Span.t ->
  ?signal:Signal.t ->
  Core.Pid.t
  -> unit

val run :
  ?timeout:Time.Span.t
  -> ?use_extra_path:bool
  -> ?working_dir:string
  -> ?setuid:int
  -> ?setgid:int
  -> ?env:([`Extend of (string * string) list
          | `Replace of (string * string) list])
  -> ?input:string
  -> ?keep_open:bool
  -> ?stdoutf:(string -> int -> unit)
  -> ?stderrf:(string -> int -> unit)
  -> ?tail_len:int
  -> prog:string
  -> args:string list
  -> unit
  -> Command_result.t
