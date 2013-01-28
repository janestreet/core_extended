open Core.Std

type slot = {
  lock_file : string;
  name : string;
  redirect_stdout : Daemon.redirect_fds;
  redirect_stderr : Daemon.redirect_fds;
}

module type T = sig
  val slot_spec : unit -> (slot -> 'm, 'm) Core_command.Spec.t
  type main
  val main_spec : (foreground:bool -> main, unit) Core_command.Spec.t
  val main : slot -> main
end

type t = (module T)

val start   : t -> Core_command.t
val stop    : t -> Core_command.t
val status  : t -> Core_command.t
val restart : t -> Core_command.t
val group   : t -> summary:string -> Core_command.t

