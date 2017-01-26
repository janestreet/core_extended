open! Core

type slot = {
  lock_file : string;
  name : string;
  redirect_stdout : Daemon.Fd_redirection.t;
  redirect_stderr : Daemon.Fd_redirection.t;
}

module type T = sig
  val slot_spec : unit -> (slot -> 'm, 'm) Command.Spec.t
  type main
  val main_spec : (foreground:bool -> main, unit -> unit) Command.Spec.t
  val main : slot -> main
end

type t = (module T)

val start   : t -> Command.t
val stop    : t -> Command.t
val status  : t -> Command.t
val restart : t -> Command.t
val group   : t -> summary:string -> Command.t

(** [acquire_lock_exn slot] locks [slot].  This can be used from within another program to
    ensure that no server is running while, e.g., an offline backup is run.

    Due to the semantics of the underlying [Lock_file.create] call, this lock is only
    released when the process exits.  To release earlier, delete the lock file manually.
 **)
val acquire_lock_exn : slot -> bool
