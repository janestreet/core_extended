open Core.Std

type slot = {
  lock_file : string;
  name : string;
  redirect_stdout : Daemon.Fd_redirection.t;
  redirect_stderr : Daemon.Fd_redirection.t;
}

module type T = sig
  val slot_spec : unit -> (slot -> 'm, 'm) Command.Spec.t
  type main
  val main_spec : (foreground:bool -> main, unit) Command.Spec.t
  val main : slot -> main
end

type t = (module T)

val start   : t -> Command.t
val stop    : t -> Command.t
val status  : t -> Command.t
val restart : t -> Command.t
val group   : t -> summary:string -> Command.t

