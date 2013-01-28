(** Interface to Unix utility functions *)

open Core.Std
open Unix

(** {2 Handling RAM limits} *)

(** [physical_ram ()] @return the total amount of physical RAM in bytes. *)
val physical_ram : unit -> int64

(** [ram_limit_spec] command line arguments to set ram limits. *)
val ram_limit_spec : Arg.t


(** {2 Signal handling} *)

(** [wrap_block_signals f] blocks all signals before execution of [f], and
    restores them afterwards. *)
val wrap_block_signals : (unit -> 'a) -> 'a

(** [ensure_at_exit ()]: catch all signals, run at_exit functions,
    then re-deliver the signal to self to ensure the default behavior.
    at_exit functions are honored only when terminating by exit, not by signals,
    so we need to do some tricks to get it run by signals too*)
val ensure_at_exit : unit -> unit

(** [get_ppids pid] returns the list of parent pids, up to init (1) for pid. *)
val get_ppids : Pid.t -> Pid.t list option
