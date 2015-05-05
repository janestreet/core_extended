open Core.Std
(** Extensions to [Core.Core_thread] *)

(** Behaves like [Thread.create] but exits the program if an exception trickles
    to the toplevel. This is generally a safer alternative. *)
val safe_create : (unit -> unit) -> Thread.t
